#
# IRT Analysis API for DLTPT
# 2PL / Rasch via TAM, with teacher-facing interpretations
# v1.2 -- steps 1-5: harvest, CTT, information, config/governance, model gate
#

library(plumber)
library(jsonlite)
library(TAM)


# =========================================================================
# Config (step 4): defaults in code, overridable per-request via body$config
# =========================================================================

DEFAULTS <- list(
  min_n_floor  = 20,      # absolute floor; below this -> error, any model
  rasch_min_n  = 20,      # Rasch usable from the floor up
  twopl_min_n  = 200,     # auto-selection switches to 2PL at/above this N
  q3_threshold = 0.2,     # |Q3| above this is reported as a flagged pair
  flag = list(
    weak_disc     = 0.6,  # discrimination below -> WeakDisc
    unstable_disc = 3.0,  # discrimination above -> UnstableDisc (edge/degenerate)
    too_easy      = 0.95, # prop-correct above -> TooEasy
    too_hard      = 0.20, # prop-correct below -> TooHard
    low_corr      = 0.30, # item-total below (and >=0) -> LowCorr
    infit_high    = 1.3,
    outfit_high   = 2.0
  ),
  theta_grid = list(min = -4, max = 4, n = 100),
  cut_theta  = NULL       # SEM-at-cut only computed when supplied
)

# Shallow-recursive overlay: keeps DEFAULTS keys, replaces where override sets.
merge_config <- function(defaults, override) {
  if (is.null(override)) return(defaults)
  for (k in names(override)) {
    if (is.list(defaults[[k]]) && is.list(override[[k]])) {
      defaults[[k]] <- merge_config(defaults[[k]], override[[k]])
    } else if (!is.null(override[[k]])) {
      defaults[[k]] <- override[[k]]
    }
  }
  defaults
}


# =========================================================================
# Parsing / shaping helpers (unchanged from v1.1)
# =========================================================================

parse_body <- function(req) {
  tryCatch(fromJSON(req$postBody), error = function(e) NULL)
}

matrix_to_df <- function(matrix_obj) {
  tryCatch({
    rows_list <- if (is.list(matrix_obj$rows[[1]])) matrix_obj$rows else list(matrix_obj$rows)
    mat <- do.call(rbind, rows_list)
    df  <- as.data.frame(mat)
    colnames(df) <- matrix_obj$columns
    df
  }, error = function(e) NULL)
}

strip_id_columns <- function(df) {
  id_names <- c("person_id", "student_id", "id", "user_id")
  id_idx   <- which(tolower(colnames(df)) %in% id_names)
  if (length(id_idx) > 0) df[, -id_idx, drop = FALSE] else df
}

na_false <- function(x) { x[is.na(x)] <- FALSE; x }

response_diagnostics <- function(resp) {
  n_total    <- nrow(resp)
  total      <- rowSums(resp, na.rm = TRUE)
  n_items    <- ncol(resp)
  perfect    <- total == n_items
  zero       <- total == 0
  incomplete <- rowSums(is.na(resp)) > 0
  list(
    n_total_attempts = n_total,
    n_perfect_scores = sum(perfect),
    n_zero_scores    = sum(zero),
    n_incomplete     = sum(incomplete),
    n_used_for_calib = sum(!(perfect | zero | incomplete))
  )
}


# =========================================================================
# Model fitting + selection (step 5)
# =========================================================================

fit_2pl <- function(resp) {
  tryCatch(
    suppressMessages(tam.mml.2pl(resp, verbose = FALSE)),
    error = function(e) structure(list(error = e$message), class = "tam_error")
  )
}

fit_rasch <- function(resp) {
  tryCatch(
    suppressMessages(tam.mml(resp, verbose = FALSE)),
    error = function(e) structure(list(error = e$message), class = "tam_error")
  )
}

# Decide model. Explicit request overrides the N gate but still obeys the floor
# (enforced upstream). Returns list(model, selection).
select_model <- function(n_cal, requested, cfg) {
  if (!is.null(requested) && nzchar(requested)) {
    m <- tolower(requested)
    if (m %in% c("rasch", "1pl"))      return(list(model = "rasch", selection = "explicit"))
    if (m %in% c("2pl"))               return(list(model = "2pl",   selection = "explicit"))
    return(list(model = NA, selection = "invalid"))
  }
  if (n_cal >= cfg$twopl_min_n) list(model = "2pl",   selection = "auto")
  else                         list(model = "rasch", selection = "auto")
}

# Model-aware item parameters. FIX #1 lives here now (single source of truth):
# 2PL difficulty is b = xsi / a; Rasch fixes a = 1 so b = xsi.
extract_item_params <- function(mod, model_type) {
  xsi <- mod$xsi$xsi
  if (model_type == "2pl") {
    a <- mod$B[, , 1][, 2]
    b <- xsi / a
  } else {
    a <- rep(1, length(xsi))
    b <- xsi
  }
  list(a = as.numeric(a), b = as.numeric(b))
}


# =========================================================================
# Item-level flags (step 4: thresholds from cfg)
# =========================================================================

build_item_flags <- function(resp, a, b, mod, cfg) {
  prop_correct <- colMeans(resp, na.rm = TRUE)
  total_score  <- rowSums(resp, na.rm = TRUE)
  item_total_cor <- sapply(resp, function(x) {
    suppressWarnings(cor(x, total_score - x, use = "pairwise.complete.obs"))
  })

  fit    <- tam.fit(mod)$itemfit
  outfit <- fit$Outfit
  infit  <- fit$Infit

  flags <- data.frame(
    Item           = colnames(resp),
    Difficulty     = round(b, 3),
    Discrimination = round(a, 3),
    PropCorrect    = round(prop_correct,   3),
    ItemTotalCor   = round(item_total_cor, 3),
    Infit          = round(infit,          3),
    Outfit         = round(outfit,         3),
    stringsAsFactors = FALSE
  )

  fc <- cfg$flag
  flags$Flag <- ""
  add <- function(mask, tag) {
    m <- na_false(mask)
    flags$Flag[m] <<- trimws(paste(flags$Flag[m], tag))
  }

  add(flags$Discrimination < 0 | flags$ItemTotalCor < 0,                 "BROKEN_negDisc")
  add(flags$Discrimination >= 0 & flags$Discrimination < fc$weak_disc,   "WeakDisc")
  add(flags$Discrimination > fc$unstable_disc,                           "UnstableDisc")
  add(flags$PropCorrect > fc$too_easy,                                   "TooEasy")
  add(flags$PropCorrect < fc$too_hard,                                   "TooHard")
  add(flags$ItemTotalCor < fc$low_corr & flags$ItemTotalCor >= 0,        "LowCorr")
  add(flags$Infit  > fc$infit_high,                                      "InfitHigh")
  add(flags$Outfit > fc$outfit_high,                                     "OutfitHigh")

  flags$Flag <- trimws(flags$Flag)
  flags
}

# Static legend so the frontend can render tiers (step 4).
flag_legend <- function() {
  list(
    list(code = "BROKEN_negDisc", tier = "severe", text = "Negative discrimination or item-total correlation; item likely broken or mis-keyed."),
    list(code = "UnstableDisc",   tier = "severe", text = "Discrimination ran to the estimator's edge; near-Guttman, unstable across samples."),
    list(code = "WeakDisc",       tier = "weak",   text = "Low discrimination; separates students poorly."),
    list(code = "TooEasy",        tier = "weak",   text = "Very high proportion correct; little information."),
    list(code = "TooHard",        tier = "weak",   text = "Very low proportion correct; little information."),
    list(code = "LowCorr",        tier = "weak",   text = "Low (non-negative) item-total correlation."),
    list(code = "InfitHigh",      tier = "weak",   text = "Infit above bound; responses noisier than the model expects."),
    list(code = "OutfitHigh",     tier = "weak",   text = "Outfit above bound; sensitive to unexpected responses by off-target students.")
  )
}


# =========================================================================
# CTT (step 2)
# =========================================================================

compute_ctt <- function(resp) {
  k <- ncol(resp)
  total <- rowSums(resp, na.rm = TRUE)
  item_var  <- apply(resp, 2, var, na.rm = TRUE)
  total_var <- var(total, na.rm = TRUE)
  alpha <- if (!is.na(total_var) && total_var > 0 && k > 1) {
    (k / (k - 1)) * (1 - sum(item_var, na.rm = TRUE) / total_var)
  } else NA_real_
  list(
    cronbach_alpha = if (is.na(alpha)) NA else round(alpha, 3),
    alpha_note     = "Equivalent to KR-20 for dichotomous items.",
    total_mean     = round(mean(total, na.rm = TRUE), 3),
    total_sd       = round(sd(total,   na.rm = TRUE), 3),
    n_items        = k,
    n_persons      = nrow(resp)
  )
}


# =========================================================================
# Local dependence (step 1: full matrix + configurable thr + MADaQ3)
# =========================================================================

compute_q3 <- function(mod, cfg) {
  empty_pairs <- data.frame(Item1 = character(0), Item2 = character(0),
                            Q3 = numeric(0), Recommendation = character(0),
                            stringsAsFactors = FALSE)
  q3_result <- tryCatch(suppressWarnings(tam.modelfit(mod, progress = FALSE)),
                        error = function(e) NULL)
  if (is.null(q3_result) || is.null(q3_result$Q3.matr)) {
    return(list(pairs = empty_pairs, matrix = list(), madaq3 = list(madaq3 = NA, maxaq3 = NA, note = "Q3 unavailable")))
  }

  q3_mat <- q3_result$Q3.matr
  ut     <- upper.tri(q3_mat)
  idx    <- which(ut, arr.ind = TRUE)          # column-major, aligns with q3_mat[ut]
  vals   <- q3_mat[ut]

  # Full upper-triangle, long form (every pair, not just flagged).
  full <- data.frame(
    Item1 = colnames(q3_mat)[idx[, 1]],
    Item2 = colnames(q3_mat)[idx[, 2]],
    Q3    = round(vals, 3),
    stringsAsFactors = FALSE
  )

  # Flagged pairs above the configured threshold (backward-compatible shape).
  hit <- which(abs(vals) > cfg$q3_threshold)
  pairs <- if (length(hit) == 0) empty_pairs else data.frame(
    Item1 = colnames(q3_mat)[idx[hit, 1]],
    Item2 = colnames(q3_mat)[idx[hit, 2]],
    Q3    = round(vals[hit], 3),
    Recommendation = "Items may share construct-irrelevant variance; consider revising one.",
    stringsAsFactors = FALSE
  )

  # Marais aQ3 centering: aQ3 = Q3 - mean(Q3); MADaQ3 = mean(|aQ3|).
  mean_q3 <- mean(vals, na.rm = TRUE)
  aq3     <- vals - mean_q3
  madaq3  <- list(
    madaq3    = round(mean(abs(aq3), na.rm = TRUE), 4),
    maxaq3    = round(max(abs(aq3),  na.rm = TRUE), 4),
    mean_q3   = round(mean_q3, 4),
    threshold = cfg$q3_threshold,
    note      = "aQ3 = Q3 - mean(Q3); MADaQ3 = mean(|aQ3|)."
  )

  list(pairs = pairs, matrix = full, madaq3 = madaq3)
}


# =========================================================================
# WLE ability (step 1: return per-person theta + SE, not just aggregate)
# =========================================================================

wle_summary <- function(mod) {
  res   <- tam.wle(mod, progress = FALSE)
  theta <- res$theta
  se    <- res$error
  m <- mean(theta, na.rm = TRUE)
  s <- sd(theta,   na.rm = TRUE)
  v <- var(theta,  na.rm = TRUE)
  rel <- if (!is.na(v) && v > 0) 1 - mean(se^2, na.rm = TRUE) / v else NA_real_
  rel <- if (!is.na(rel) && rel < 0) 0 else rel
  list(mean = m, sd = s, reliability = rel,
       theta = as.numeric(theta), se = as.numeric(se))
}

# Person-fit (step 5). Fail-safe: any failure returns NULL, never crashes run.
person_fit_table <- function(mod, orig_rows) {
  pf <- tryCatch(suppressWarnings(tam.personfit(mod)), error = function(e) NULL)
  if (is.null(pf)) return(NULL)
  cn <- colnames(pf)
  pick <- function(kw) {
    cand <- cn[grepl(kw, cn, ignore.case = TRUE) & !grepl("_t$|\\.t$", cn)]
    if (length(cand)) cand[1] else NA
  }
  ic <- pick("infit"); oc <- pick("outfit")
  n  <- min(nrow(pf), length(orig_rows))
  lapply(seq_len(n), function(i) list(
    row    = orig_rows[i],
    infit  = if (!is.na(ic)) round(as.numeric(pf[[ic]][i]), 3) else NA,
    outfit = if (!is.na(oc)) round(as.numeric(pf[[oc]][i]), 3) else NA
  ))
}


# =========================================================================
# Curves + information (step 3): ICC probability, item info, test info/SEM
# =========================================================================

build_curves <- function(a, b, item_names, grid) {
  theta <- seq(grid$min, grid$max, length.out = grid$n)
  icc <- lapply(seq_along(item_names), function(i) {
    P <- 1 / (1 + exp(-a[i] * (theta - b[i])))
    I <- a[i]^2 * P * (1 - P)
    list(item = item_names[i], theta = theta,
         probability = round(P, 4), information = round(I, 4))
  })
  Imat <- sapply(seq_along(item_names), function(i) {
    P <- 1 / (1 + exp(-a[i] * (theta - b[i]))); a[i]^2 * P * (1 - P)
  })
  tif <- rowSums(Imat)
  sem <- ifelse(tif > 0, 1 / sqrt(tif), NA_real_)
  list(
    icc = icc,
    test_information = list(theta = theta,
                           information = round(tif, 4),
                           sem = round(sem, 4))
  )
}

info_at_cut <- function(a, b, cut) {
  if (is.null(cut) || is.na(cut)) return(list(provided = FALSE))
  P <- 1 / (1 + exp(-a * (cut - b)))
  I <- sum(a^2 * P * (1 - P))
  list(provided = TRUE, theta = cut,
       information = round(I, 4),
       sem = if (I > 0) round(1 / sqrt(I), 4) else NA)
}


# =========================================================================
# Teacher-facing text generators (unchanged)
# =========================================================================

text_eap <- function(r) {
  if (is.na(r))     "EAP reliability could not be estimated from this sample."
  else if (r < 0.7) "EAP reliability is below 0.7. The test cannot reliably separate students."
  else if (r < 0.8) "EAP reliability is close to 0.8, indicating acceptable reliability."
  else if (r < 0.9) "EAP reliability is above 0.8, indicating good reliability."
  else              "EAP reliability is 0.9 or above, indicating excellent reliability."
}

text_wle_rel <- function(r) {
  if (is.na(r))     "WLE reliability could not be estimated from this sample."
  else if (r < 0.7) "WLE reliability is below 0.7, indicating low reliability."
  else if (r < 0.8) "WLE reliability is close to 0.8, indicating acceptable reliability."
  else if (r < 0.9) "WLE reliability is above 0.8, indicating good reliability."
  else              "WLE reliability is 0.9 or above, indicating excellent reliability."
}

text_mean <- function(m) {
  if (is.na(m))     "Mean student ability could not be estimated."
  else if (m >  1)  "Mean ability is above 1: the test appears easy for this group."
  else if (m < -1)  "Mean ability is below -1: the test appears difficult for this group."
  else              "Mean ability is close to 0: the test is suitable for most students."
}

text_sd <- function(s) {
  if (is.na(s))     "Ability spread could not be estimated."
  else if (s < 0.5) "Abilities are very similar; the test does not differentiate students well."
  else if (s < 0.8) "Abilities show some variation, but differences are still small."
  else if (s < 1.3) "Healthy spread in abilities; the test differentiates well between students."
  else              "Abilities vary substantially, indicating large performance differences."
}

text_ability <- function(m) {
  if (is.na(m))      "Cannot determine target ability level from this sample."
  else if (m >  1)   "The exercise targets high-ability students."
  else if (m < -0.5) "The exercise targets low-ability students."
  else               "The exercise targets average-ability students."
}

text_variance <- function(s) {
  if (!is.na(s) && s < 0.5)
    "Consider adding items that vary more in difficulty to better distinguish students."
  else ""
}

text_improvement <- function(r) {
  if (is.na(r) || r < 0.7) {
    paste(
      "To improve reliability, consider:",
      "- Adding more discriminating items (discrimination > 0.6)",
      "- Removing items that are too easy (> 95% correct) or too hard (< 20% correct)",
      "- Increasing content diversity to raise score variance.",
      sep = "\n"
    )
  } else ""
}

build_teacher_message <- function(mean_wle, sd_wle, wle_rel) {
  parts <- c(text_ability(mean_wle), text_wle_rel(wle_rel),
             text_variance(sd_wle), text_improvement(wle_rel))
  paste(parts[nzchar(parts)], collapse = "\n")
}


# =========================================================================
# Analysis core -- callable offline for smoke tests (thin route below)
# =========================================================================
# Returns a result list. On failure, returns list(error=..., status_code=NNN).

run_analysis <- function(body) {

  cfg <- merge_config(DEFAULTS, body$config)

  if (is.null(body$matrix$rows) || length(body$matrix$rows) == 0)
    return(list(error = "Empty matrix in payload.", status_code = 400))

  df <- matrix_to_df(body$matrix)
  if (is.null(df))
    return(list(error = "Could not convert matrix to data.frame.", status_code = 400))

  resp <- strip_id_columns(df)
  if (ncol(resp) < 3)
    return(list(error = "Too few items for IRT analysis (need at least 3).", status_code = 400))
  if (nrow(resp) < cfg$min_n_floor)
    return(list(error = sprintf("Too few respondents (need at least %d).", cfg$min_n_floor),
                status_code = 400))

  resp[] <- lapply(resp, function(x) as.numeric(as.character(x)))

  diag_info <- response_diagnostics(resp)

  # FIX #2: drop perfect / zero / incomplete BEFORE calibration.
  total    <- rowSums(resp, na.rm = TRUE)
  keep_row <- !(total == 0 | total == ncol(resp) | rowSums(is.na(resp)) > 0)
  resp_cal <- resp[keep_row, , drop = FALSE]
  orig_rows <- which(keep_row)

  n_cal <- nrow(resp_cal)
  if (n_cal < cfg$min_n_floor)
    return(list(error = sprintf(
      "Too few respondents after removing perfect/zero/incomplete scores (need at least %d).",
      cfg$min_n_floor), status_code = 400))

  # ---- Model selection (step 5) ----
  sel <- select_model(n_cal, body$model, cfg)
  if (is.na(sel$model))
    return(list(error = "Invalid 'model'; expected 'rasch'/'1pl' or '2pl'.", status_code = 400))
  if (sel$model == "2pl" && sel$selection == "explicit" && n_cal < cfg$min_n_floor)
    return(list(error = "Too few respondents for 2PL.", status_code = 400))

  mod <- if (sel$model == "2pl") fit_2pl(resp_cal) else fit_rasch(resp_cal)
  if (inherits(mod, "tam_error"))
    return(list(error = paste("TAM", toupper(sel$model), "model failed:", mod$error),
                status_code = 500))

  pars <- extract_item_params(mod, sel$model)

  # ---- Metrics ----
  item_flags <- build_item_flags(resp_cal, pars$a, pars$b, mod, cfg)
  ctt        <- compute_ctt(resp_cal)
  q3         <- compute_q3(mod, cfg)
  wle        <- wle_summary(mod)
  curves     <- build_curves(pars$a, pars$b, colnames(resp_cal), cfg$theta_grid)
  cut_info   <- info_at_cut(pars$a, pars$b, cfg$cut_theta)
  pfit       <- person_fit_table(mod, orig_rows)

  eap_rel <- round(as.numeric(mod$EAP.rel), 3)
  wle_rel <- if (!is.na(wle$reliability)) round(wle$reliability, 3) else NA

  ability <- lapply(seq_along(orig_rows), function(i) list(
    row = orig_rows[i], theta = round(wle$theta[i], 3), se = round(wle$se[i], 3)
  ))

  teacher_msg <- build_teacher_message(wle$mean, wle$sd, wle$reliability)

  list(
    model = list(
      type            = if (sel$model == "2pl") "2PL" else "Rasch",
      package         = "TAM",
      selection       = sel$selection,
      n_used_for_calib = n_cal,
      eap_reliability = eap_rel,        eap_text      = text_eap(eap_rel),
      wle_reliability = wle_rel,        wle_text      = text_wle_rel(wle_rel),
      wle_mean        = round(wle$mean, 3), wle_mean_text = text_mean(wle$mean),
      wle_sd          = round(wle$sd, 3),   wle_sd_text   = text_sd(wle$sd)
    ),
    ctt                  = ctt,
    items                = item_flags,
    flag_legend          = flag_legend(),
    ability              = ability,
    person_fit           = pfit,
    correlations         = q3$pairs,      # backward-compatible flagged pairs
    q3_matrix            = q3$matrix,     # full upper triangle
    q3_madaq3            = q3$madaq3,
    icc                  = curves$icc,    # now includes per-item information
    test_information     = curves$test_information,
    cut                  = cut_info,
    diagnostics          = diag_info,
    config_used          = cfg,
    message_for_teachers = teacher_msg,
    metadata             = body$metadata
  )
}


# =========================================================================
# API (thin transport over run_analysis)
# =========================================================================

#* @apiTitle IRT Analysis for DLTPT
#* @apiDescription 2PL/Rasch IRT analysis, CTT, information, diagnostics, and teacher-facing interpretations.

#* Run IRT analysis on a response matrix
#* @post /irt
#* @post /irt/
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body)) {
    res$status <- 400
    return(list(error = "Invalid JSON in request body."))
  }
  out <- run_analysis(body)
  if (!is.null(out$error)) {
    res$status <- if (!is.null(out$status_code)) out$status_code else 400
    return(list(error = out$error))
  }
  out
}

#* Health check
#* @get /
function() {
  list(status = "ok", service = "DLTPT IRT", version = "1.2")
}
