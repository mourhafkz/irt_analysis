#
# IRT Analysis API for DLTPT  --  v2.3
# =====================================================================
# One TAM/plumber service, two engines:
#
#   POST /irt   Per-exercise IRT. 2PL/Rasch with N-gated auto-selection,
#               CTT (Cronbach a), item flags, Yen's Q3 + MADaQ3, per-person
#               WLE theta+SE, person-fit, ICC + item/test information + SEM,
#               SEM-at-cut, and teacher-facing interpretations.
#
#   POST /form  Form-scope multi-method + Layer C. Runs several methods over
#               a set of exercises taken by one roster and compares them:
#               item-level 2PL/Rasch and exercise-level PCM, with cross-method
#               reliability, theta concordance, classification-at-cut, and
#               item-difficulty concordance. Exercise-type-agnostic; models
#               NA as not-administered rather than dropping it.
#
# Shared helpers (config, model fits, WLE, item params) serve both engines.
# Per-request overrides via body$config; see DEFAULTS.
#
# v2.1 adds an extended-diagnostics + standard-setting layer (bottom of
# file): local dependence within vs between exercise, model comparison
# (AIC/BIC), TCC linking, grain-size Angoff standard setting + G-theory,
# dimensionality, and DIF. /form now emits local_dependence,
# model_comparison, tcc, and dimensionality inline; new routes:
# /standard_set, /ld, /dimensionality, /dif.
# =====================================================================

library(plumber)
library(jsonlite)
library(TAM)
library(mirt)     # bifactor (v2.2)
library(mokken)   # nonparametric scaling (v2.2)


# =====================================================================
# SHARED FOUNDATION
# =====================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

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
  cut_theta  = NULL       # SEM-at-cut / classification only when supplied
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

parse_body <- function(req) {
  tryCatch(fromJSON(req$postBody), error = function(e) NULL)
}

na_false <- function(x) { x[is.na(x)] <- FALSE; x }

# ---- model fits ------------------------------------------------------
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

fit_pcm <- function(resp) {
  tryCatch(
    suppressMessages(tam.mml(resp, irtmodel = "PCM", verbose = FALSE)),
    error = function(e) structure(list(error = e$message), class = "tam_error")
  )
}

fit_gpcm <- function(resp) {
  tryCatch(
    suppressMessages(tam.mml.2pl(resp, irtmodel = "GPCM", verbose = FALSE)),
    error = function(e) structure(list(error = e$message), class = "tam_error")
  )
}

# Model-aware item parameters (single source of truth for FIX #1):
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

# WLE summary + per-person theta/SE (used by both engines).
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


# =====================================================================
# PER-EXERCISE ENGINE  (/irt)
# =====================================================================

# ---- payload shaping (matrix-style body) -----------------------------
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

# Decide model. Explicit request overrides the N gate but still obeys the floor.
select_model <- function(n_cal, requested, cfg) {
  twopl_min <- cfg$twopl_min_n %||% 200
  if (!is.null(requested) && nzchar(requested)) {
    m <- tolower(requested)
    if (m %in% c("rasch", "1pl"))
      return(list(model = "rasch", selection = "explicit", warning = NULL))
    if (m %in% c("2pl")) {
      warn <- if (n_cal < twopl_min)
        sprintf(paste("2PL requested with N=%d < recommended %d; slope estimates",
                      "may be unstable. Rasch is advised at this sample size."),
                n_cal, twopl_min) else NULL
      return(list(model = "2pl", selection = "explicit", warning = warn))
    }
    return(list(model = NA, selection = "invalid", warning = NULL))
  }
  if (n_cal >= twopl_min) list(model = "2pl",   selection = "auto", warning = NULL)
  else                    list(model = "rasch", selection = "auto", warning = NULL)
}

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

  add(flags$Discrimination < 0 | flags$ItemTotalCor < 0,               "BROKEN_negDisc")
  add(flags$Discrimination >= 0 & flags$Discrimination < fc$weak_disc, "WeakDisc")
  add(flags$Discrimination > fc$unstable_disc,                         "UnstableDisc")
  add(flags$PropCorrect > fc$too_easy,                                 "TooEasy")
  add(flags$PropCorrect < fc$too_hard,                                 "TooHard")
  add(flags$ItemTotalCor < fc$low_corr & flags$ItemTotalCor >= 0,      "LowCorr")
  add(flags$Infit  > fc$infit_high,                                    "InfitHigh")
  add(flags$Outfit > fc$outfit_high,                                   "OutfitHigh")

  flags$Flag <- trimws(flags$Flag)
  flags
}

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

compute_q3 <- function(mod, cfg) {
  empty_pairs <- data.frame(Item1 = character(0), Item2 = character(0),
                            Q3 = numeric(0), Recommendation = character(0),
                            stringsAsFactors = FALSE)
  q3_result <- tryCatch(suppressWarnings(tam.modelfit(mod, progress = FALSE)),
                        error = function(e) NULL)
  if (is.null(q3_result) || is.null(q3_result$Q3.matr)) {
    return(list(pairs = empty_pairs, matrix = list(),
                madaq3 = list(madaq3 = NA, maxaq3 = NA, note = "Q3 unavailable")))
  }

  q3_mat <- q3_result$Q3.matr
  ut     <- upper.tri(q3_mat)
  idx    <- which(ut, arr.ind = TRUE)          # column-major, aligns with q3_mat[ut]
  vals   <- q3_mat[ut]

  full <- data.frame(
    Item1 = colnames(q3_mat)[idx[, 1]],
    Item2 = colnames(q3_mat)[idx[, 2]],
    Q3    = round(vals, 3),
    stringsAsFactors = FALSE
  )

  hit <- which(abs(vals) > cfg$q3_threshold)
  pairs <- if (length(hit) == 0) empty_pairs else data.frame(
    Item1 = colnames(q3_mat)[idx[hit, 1]],
    Item2 = colnames(q3_mat)[idx[hit, 2]],
    Q3    = round(vals[hit], 3),
    Recommendation = "Items may share construct-irrelevant variance; consider revising one.",
    stringsAsFactors = FALSE
  )

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

# ---- teacher-facing text generators ----------------------------------
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

# ---- /irt analysis core (offline-testable) ---------------------------
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
      type             = if (sel$model == "2pl") "2PL" else "Rasch",
      package          = "TAM",
      selection        = sel$selection,
      sample_size_warning = sel$warning,
      n_used_for_calib = n_cal,
      eap_reliability  = eap_rel,           eap_text      = text_eap(eap_rel),
      wle_reliability  = wle_rel,           wle_text      = text_wle_rel(wle_rel),
      wle_mean         = round(wle$mean, 3), wle_mean_text = text_mean(wle$mean),
      wle_sd           = round(wle$sd, 3),   wle_sd_text   = text_sd(wle$sd)
    ),
    ctt                  = ctt,
    items                = item_flags,
    flag_legend          = flag_legend(),
    ability              = ability,
    person_fit           = pfit,
    correlations         = q3$pairs,      # backward-compatible flagged pairs
    q3_matrix            = q3$matrix,     # full upper triangle
    q3_madaq3            = q3$madaq3,
    icc                  = curves$icc,    # per-item ICC + information
    test_information     = curves$test_information,
    cut                  = cut_info,
    diagnostics          = diag_info,
    config_used          = cfg,
    message_for_teachers = teacher_msg,
    metadata             = body$metadata
  )
}


# =====================================================================
# FORM-SCOPE ENGINE  (/form)  --  multi-method + Layer C
# =====================================================================
# Payload (from build_form_matrices; exercise-type-agnostic):
#   { person_keys,
#     item:{columns, exercise_id, rows},        # person x item (dichotomous)
#     exercise:{columns, max_scores, rows},     # person x exercise (0..k sum)
#     exercises, coverage, methods, config }
# Cells may be null -> NA = not-administered (MODELLED, not dropped).

# ---- payload block -> numeric matrix (nulls -> NA) -------------------
block_to_matrix <- function(block) {
  rows <- block$rows
  if (is.matrix(rows)) {
    m <- rows; storage.mode(m) <- "numeric"
  } else {
    m <- do.call(rbind, lapply(rows, function(r)
      vapply(r, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))))
  }
  colnames(m) <- block$columns
  m
}

# NA-aware cleaning: drop only all-NA rows and extreme-on-observed rows.
clean_matrix <- function(resp, max_scores = NULL) {
  vapply(seq_len(nrow(resp)), function(i) {
    row   <- resp[i, ]
    obs   <- !is.na(row)
    n_obs <- sum(obs)
    if (n_obs == 0) return(FALSE)
    rsum <- sum(row[obs])
    rmax <- if (is.null(max_scores)) n_obs else sum(max_scores[obs])
    !(rsum == 0 || rsum == rmax)
  }, logical(1))
}

# Native item params per method (best-effort; never fails the run).
extract_items_generic <- function(mod, method, item_names) {
  if (method %in% c("item_2pl", "item_rasch")) {
    p <- extract_item_params(mod, if (method == "item_2pl") "2pl" else "rasch")
    items <- lapply(seq_along(item_names), function(i)
      list(item = item_names[i], b = round(p$b[i], 3), a = round(p$a[i], 3)))
    ord <- item_names[order(p$b)]
  } else {                                   # PCM: per-exercise mean threshold
    th <- tryCatch(tam.threshold(mod), error = function(e) NULL)
    bvec <- if (!is.null(th)) rowMeans(as.matrix(th), na.rm = TRUE)
            else rep(NA_real_, length(item_names))
    items <- lapply(seq_along(item_names), function(i)
      list(item = item_names[i], b = round(bvec[i], 3)))
    ord <- item_names[order(bvec)]
  }
  list(items = items, order = ord)
}

# Run one method -> COMMON OUTPUT CONTRACT (or {method, error}).
# {method, scope, model, n_used, person_keys_used, theta, se,
#  reliability:{eap,wle}, items, item_order}
run_method <- function(method, item_m, exercise_m, exercise_max, person_keys, cfg,
                       item_max = NULL) {
  if (method %in% c("item_2pl", "item_rasch")) {
    resp <- item_m; max_scores <- NULL; scope <- "item"
  } else if (method %in% c("item_pcm", "item_gpcm")) {
    resp <- item_m; max_scores <- item_max; scope <- "item"
  } else if (method == "exercise_pcm") {
    resp <- exercise_m; max_scores <- exercise_max; scope <- "exercise"
  } else {
    return(list(method = method, error = "unknown method"))
  }
  if (is.null(resp) || ncol(resp) < 2)
    return(list(method = method, error = "matrix unavailable or < 2 columns"))

  keep   <- clean_matrix(resp, max_scores)
  resp_c <- resp[keep, , drop = FALSE]
  pk_c   <- as.character(person_keys)[keep]

  floor_n <- cfg$min_n_floor %||% 20
  if (nrow(resp_c) < floor_n)
    return(list(method = method,
                error = sprintf("too few respondents after cleaning (%d < %d)",
                                nrow(resp_c), floor_n)))

  mod <- switch(method,
    item_2pl     = fit_2pl(resp_c),
    item_rasch   = fit_rasch(resp_c),
    item_pcm     = fit_pcm(resp_c),
    item_gpcm    = fit_gpcm(resp_c),
    exercise_pcm = fit_pcm(resp_c))
  if (inherits(mod, "tam_error"))
    return(list(method = method, error = paste("fit failed:", mod$error)))

  wle <- wle_summary(mod)
  eap <- tryCatch(round(as.numeric(mod$EAP.rel), 3), error = function(e) NA_real_)

  theta <- as.numeric(wle$theta); se <- as.numeric(wle$se)
  names(theta) <- pk_c; names(se) <- pk_c

  it <- tryCatch(extract_items_generic(mod, method, colnames(resp_c)),
                 error = function(e) list(items = NULL, order = NULL))

  list(
    method = method, scope = scope,
    model  = switch(method, item_2pl = "2PL", item_rasch = "Rasch",
                    item_pcm = "PCM (item)", item_gpcm = "GPCM (item)", exercise_pcm = "PCM"),
    n_used = nrow(resp_c),
    person_keys_used = pk_c,
    theta = as.list(theta), se = as.list(se),
    reliability = list(
      eap = eap,
      wle = if (!is.na(wle$reliability)) round(wle$reliability, 3) else NA_real_
    ),
    items = it$items, item_order = it$order,
    .theta = theta,  # out-of-band for Layer C; dropped before serialize
    .mod   = mod     # out-of-band for LD / model comparison / TCC / dimensionality
  )
}

# Manual Cohen's kappa (2x2, no new dep).
cohen_kappa <- function(a, b) {
  tab <- table(factor(a, levels = c(0, 1)), factor(b, levels = c(0, 1)))
  n <- sum(tab); if (n == 0) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / (n^2)
  if (pe == 1) return(NA_real_)
  round((po - pe) / (1 - pe), 3)
}

# Layer C: cross-method comparison over whatever methods succeeded.
layer_c <- function(results, cfg) {
  ok <- Filter(function(r) is.null(r$error), results)

  rel_tbl <- lapply(ok, function(r) list(
    method = r$method, scope = r$scope, model = r$model,
    n_used = r$n_used, eap = r$reliability$eap, wle = r$reliability$wle))

  pearson <- list(); spearman <- list(); classification <- NULL
  cut <- cfg$cut_theta
  if (!is.null(cut)) kappa <- list()

  if (length(ok) >= 2) {
    for (i in 1:(length(ok) - 1)) for (j in (i + 1):length(ok)) {
      a <- ok[[i]]; b <- ok[[j]]
      common <- intersect(names(a$.theta), names(b$.theta))
      key <- paste0(a$method, "~", b$method)
      if (length(common) >= 3) {
        ta <- a$.theta[common]; tb <- b$.theta[common]
        pearson[[key]]  <- round(cor(ta, tb), 3)
        spearman[[key]] <- round(cor(ta, tb, method = "spearman"), 3)
        if (!is.null(cut))
          kappa[[key]] <- cohen_kappa(as.integer(ta >= cut), as.integer(tb >= cut))
      } else {
        pearson[[key]] <- NA_real_; spearman[[key]] <- NA_real_
        if (!is.null(cut)) kappa[[key]] <- NA_real_
      }
    }
  }
  n_common_all <- if (length(ok) >= 2)
    length(Reduce(intersect, lapply(ok, function(r) names(r$.theta)))) else NA_integer_
  if (!is.null(cut)) classification <- list(cut = cut, kappa = kappa)

  # item difficulty concordance among item-scope methods (shared item set)
  item_ms <- Filter(function(r) r$scope == "item" && !is.null(r$items), ok)
  item_conc <- NULL
  if (length(item_ms) >= 2) {
    bmap <- lapply(item_ms, function(r)
      setNames(vapply(r$items, function(it) it$b, numeric(1)),
               vapply(r$items, function(it) it$item, character(1))))
    sp <- list()
    for (i in 1:(length(item_ms) - 1)) for (j in (i + 1):length(item_ms)) {
      ci  <- intersect(names(bmap[[i]]), names(bmap[[j]]))
      key <- paste0(item_ms[[i]]$method, "~", item_ms[[j]]$method)
      sp[[key]] <- if (length(ci) >= 3)
        round(cor(bmap[[i]][ci], bmap[[j]][ci], method = "spearman"), 3) else NA_real_
    }
    item_conc <- list(spearman_b = sp)
  }

  list(
    reliability_table = rel_tbl,
    theta_concordance = list(n_common_all = n_common_all,
                             pearson = pearson, spearman = spearman),
    classification = classification,
    item_difficulty_concordance = item_conc
  )
}

# ---- /form analysis core (offline-testable) --------------------------
run_form_analysis <- function(body) {
  cfg     <- merge_config(DEFAULTS, body$config)
  methods <- body$methods
  if (is.null(methods) || length(methods) == 0) methods <- c("item_2pl", "exercise_pcm")
  person_keys <- as.character(body$person_keys)

  item_m       <- if (!is.null(body$item))     block_to_matrix(body$item)          else NULL
  item_max     <- if (!is.null(body$item))     as.numeric(body$item$max_scores)    else NULL
  exercise_m   <- if (!is.null(body$exercise)) block_to_matrix(body$exercise)      else NULL
  exercise_max <- if (!is.null(body$exercise)) as.numeric(body$exercise$max_scores) else NULL
  # body$item$exercise_id reserved for bifactor (later method); unused here.

  results <- lapply(methods, function(m)
    run_method(m, item_m, exercise_m, exercise_max, person_keys, cfg, item_max))

  comparison <- layer_c(results, cfg)

  # --- v2.1: local dependence, model comparison, TCC, dimensionality ---
  # (all derived from the item-scope fit + exercise_id grouping)
  item_fit <- Filter(function(r) !is.null(r$.mod) && r$scope == "item", results)
  ld <- if (length(item_fit))
    ld_by_group(item_fit[[1]]$.mod, colnames(item_m), body$item$exercise_id,
                cfg$q3_threshold) else list(available = FALSE)

  fits_l <- Filter(function(r) !is.null(r$.mod), results)
  fits   <- setNames(lapply(fits_l, function(r) r$.mod),
                     vapply(fits_l, function(r) r$method, character(1)))
  scopes <- setNames(as.list(vapply(fits_l, function(r) r$scope, character(1))),
                     vapply(fits_l, function(r) r$method, character(1)))
  mc <- if (length(fits)) model_comparison(fits, scopes) else NULL

  tcc <- NULL
  if (length(item_fit) && item_fit[[1]]$method %in% c("item_2pl", "item_rasch")) {
    p  <- extract_item_params(item_fit[[1]]$.mod,
            if (item_fit[[1]]$method == "item_2pl") "2pl" else "rasch")
    gr <- seq(-4, 4, 0.05)
    tcc <- list(theta = gr, expected_raw = round(tcc_raw_from_theta(p$a, p$b, gr), 2),
                max_raw = length(p$b))
  }

  dim_rep <- if (length(item_fit))
    dimensionality_report(item_fit[[1]]$.mod, colnames(item_m), body$item$exercise_id)
    else list(available = FALSE)

  results <- lapply(results, function(r) { r$.theta <- NULL; r$.mod <- NULL; r })

  list(
    methods_requested = methods,
    results           = results,
    comparison        = comparison,
    local_dependence  = ld,
    model_comparison  = mc,
    tcc               = tcc,
    dimensionality    = dim_rep,
    coverage          = body$coverage,
    exercises         = body$exercises,
    config_used       = cfg
  )
}



# =====================================================================
# EXTENDED MODELS (v2.2): bifactor, Mokken, Mantel-Haenszel DIF
# =====================================================================

bifactor_report <- function(resp, group, item_names = colnames(resp)) {
  if (!requireNamespace("mirt", quietly = TRUE))
    return(list(available = FALSE, note = "mirt not installed"))
  X <- as.matrix(resp); X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 20 || ncol(X) < 4)
    return(list(available = FALSE, note = "need >=20 complete cases and >=4 items"))
  grp      <- as.character(group)
  specific <- as.integer(factor(grp, levels = unique(grp)))   # 1..nPassage
  tryCatch({
    bf  <- mirt::bfactor(X, model = specific, verbose = FALSE,
                         technical = list(NCYCLES = 800))
    uni <- mirt::mirt(X, 1, itemtype = "Rasch", verbose = FALSE)
    sig  <- summary(bf, verbose = FALSE)
    load <- sig$rotF %||% sig$F
    g_load <- as.numeric(load[, 1]); names(g_load) <- item_names
    spec_by_pass <- tapply(seq_along(item_names), grp, function(ix) {
      cols <- 2:ncol(load); v <- load[ix, cols]; mean(abs(v[v != 0]), na.rm = TRUE)
    })
    ic_bf <- mirt::anova(uni, bf)                # LRT + AIC/BIC on same items
    list(
      available = TRUE, n_passages = length(unique(grp)),
      general_loadings = lapply(seq_along(item_names), function(i)
        list(item = item_names[i], general = round(g_load[i], 3))),
      mean_specific_loading_by_passage = round(spec_by_pass, 3),
      empirical_reliability = tryCatch(round(as.numeric(
        mirt::empirical_rxx(mirt::fscores(bf, method = "EAP",
                                          full.scores.SE = TRUE))[1]), 3),
        error = function(e) NA_real_),
      fit_vs_unidim = list(
        bifactor_BIC     = round(ic_bf$BIC[2], 1),
        unidim_BIC       = round(ic_bf$BIC[1], 1),
        prefers_bifactor = isTRUE(ic_bf$BIC[2] < ic_bf$BIC[1]),
        lrt_p            = tryCatch(round(ic_bf$p[2], 4), error = function(e) NA_real_)),
      interpretation = paste(
        "Bifactor keeps gap-level items while a per-passage specific factor",
        "absorbs within-passage dependence. BIC prefers the simpler model when",
        "the specific factors do not earn their parameters (common at small N);",
        "a significant LRT with BIC still favouring unidim means dependence is",
        "real but modest. Compare with the PCM (exercise-scope) result."))
  }, error = function(e) list(available = FALSE,
                              note = paste("bifactor failed:", conditionMessage(e))))
}


mokken_report <- function(resp, item_names = colnames(resp)) {
  if (!requireNamespace("mokken", quietly = TRUE))
    return(list(available = FALSE, note = "mokken not installed"))
  X <- as.matrix(resp); X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 20 || ncol(X) < 3)
    return(list(available = FALSE, note = "need >=20 complete cases and >=3 items"))
  tryCatch({
    Hs <- NULL
    utils::capture.output(Hs <- mokken::coefH(X, se = TRUE))   # silence side-effect print
    Hscale <- suppressWarnings(as.numeric(Hs$H[[1]]))
    Hse    <- suppressWarnings(as.numeric(Hs$H[[2]]))
    Hi_df  <- as.data.frame(Hs$Hi, stringsAsFactors = FALSE)
    Hitem  <- suppressWarnings(as.numeric(Hi_df[[1]]))
    aisp   <- tryCatch(as.integer(mokken::aisp(X)),
                       error = function(e) rep(NA_integer_, ncol(X)))
    items <- lapply(seq_along(item_names), function(i)
      list(item = item_names[i], H = round(Hitem[i], 3),
           scale = if (length(aisp) >= i) aisp[i] else NA_integer_))
    interp <- if (is.na(Hscale)) "Scale H not estimable." else
      if (Hscale < .3) "Scale H < 0.30: items do not form a Mokken scale (weak scalability)." else
      if (Hscale < .4) "Scale H 0.30-0.40: weak but usable scale." else
      if (Hscale < .5) "Scale H 0.40-0.50: medium scale." else
                       "Scale H >= 0.50: strong scale."
    list(available = TRUE,
         scale_H = round(Hscale, 3),
         scale_H_se = if (!is.na(Hse)) round(Hse, 3) else NA_real_,
         n_scales_aisp = length(unique(stats::na.omit(aisp))),
         items = items,
         rule_of_thumb = "H<0.3 not scalable; 0.3-0.4 weak; 0.4-0.5 medium; >=0.5 strong.",
         interpretation = interp)
  }, error = function(e) list(available = FALSE,
                              note = paste("mokken failed:", conditionMessage(e))))
}


classify_ets_dif <- function(ddif, p, stable) {
  if (!isTRUE(stable) || is.na(ddif) || is.na(p)) return("unstable")
  ad <- abs(ddif); sig <- p < 0.05
  if (ad >= 1.5 && sig) "C" else if (ad >= 1.0 && sig) "B" else "A"
}


mh_dif_report <- function(resp, group_vec, n_strata = 4) {
  X <- as.matrix(resp); g <- as.factor(group_vec)
  if (nlevels(g) != 2) return(list(available = FALSE, note = "MH DIF needs exactly 2 groups"))
  ref_lab <- levels(g)[1]; foc_lab <- levels(g)[2]
  total <- rowSums(X, na.rm = TRUE); k <- ncol(X)

  per_item <- lapply(seq_len(k), function(j) {
    rest <- total - X[, j]
    qs <- unique(stats::quantile(rest, probs = seq(0, 1, length.out = n_strata + 1),
                                 na.rm = TRUE))
    strat <- if (length(qs) >= 3) cut(rest, breaks = qs, include.lowest = TRUE)
             else factor(rest)
    tab <- table(factor(X[, j], levels = c(1, 0)), g, strat)
    keep <- apply(tab, 3, function(s) all(colSums(s) > 0) && sum(s) >= 2)
    tab  <- tab[, , keep, drop = FALSE]
    if (dim(tab)[3] < 1) return(list(item = colnames(X)[j], available = FALSE))
    mh <- tryCatch(stats::mantelhaen.test(tab, correct = TRUE), error = function(e) NULL)
    if (is.null(mh) || is.null(mh$estimate)) return(list(item = colnames(X)[j], available = FALSE))
    orr    <- as.numeric(mh$estimate)
    stable <- is.finite(orr) && orr > 0
    ddif   <- if (stable) -2.35 * log(orr) else NA_real_
    list(item = colnames(X)[j], available = TRUE,
         MH_OR = if (is.finite(orr)) round(orr, 3) else NA_real_,
         MH_chisq = round(as.numeric(mh$statistic), 3),
         p = round(mh$p.value, 4),
         MH_D_DIF = if (stable) round(ddif, 3) else NA_real_,
         ETS_class = classify_ets_dif(ddif, mh$p.value, stable))
  })
  ok <- Filter(function(x) isTRUE(x$available), per_item)
  if (!length(ok)) return(list(available = FALSE, note = "no items had usable strata"))
  tab <- do.call(rbind, lapply(ok, function(x) data.frame(
    Item = x$item, MH_OR = x$MH_OR, MH_chisq = x$MH_chisq, p = x$p,
    MH_D_DIF = x$MH_D_DIF, ETS = x$ETS_class, stringsAsFactors = FALSE)))
  tab <- tab[order(-abs(replace(tab$MH_D_DIF, is.na(tab$MH_D_DIF), 0))), ]
  list(
    available = TRUE, groups = c(ref_lab, foc_lab),
    method = sprintf("Mantel-Haenszel, rest-score matched in %d quantile strata, significance-gated ETS D-DIF (A/B/C)", n_strata),
    n_items_tested = nrow(tab),
    n_unstable = sum(tab$ETS == "unstable"),
    n_BC = sum(tab$ETS %in% c("B", "C"), na.rm = TRUE),
    n_C  = sum(tab$ETS == "C", na.rm = TRUE),
    items = tab,
    note = paste("ETS: |D|<1 negligible (A); 1.0-1.5 moderate (B) and >=1.5 large (C),",
                 "each requiring MH p<.05; 'unstable' = degenerate odds ratio (too few",
                 "cases). Small per-group N reduces power; pair with the Rasch-contrast",
                 "dif_report() and prefer testlet-level DIF under within-passage dependence."))
}

# =====================================================================
# ROUTES
# =====================================================================

#* @apiTitle IRT Analysis for DLTPT
#* @apiDescription Per-exercise IRT (/irt) and form-scope multi-method + Layer C (/form).

#* Per-exercise IRT analysis on a response matrix
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

#* Form-scope multi-method + Layer C analysis
#* @post /form
#* @post /form/
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body)) { res$status <- 400; return(list(error = "Invalid JSON in request body.")) }
  if (is.null(body$person_keys) || (is.null(body$item) && is.null(body$exercise))) {
    res$status <- 400
    return(list(error = "Missing person_keys and at least one of item/exercise."))
  }
  tryCatch(
    run_form_analysis(body),
    error = function(e) { res$status <- 500;
      list(error = paste("form analysis failed:", conditionMessage(e))) }
  )
}

#* Bifactor / testlet model (gap items + per-passage specific factors)
#* @post /bifactor
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item)) {
    res$status <- 400; return(list(error = "Need item block with exercise_id."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    keep   <- clean_matrix(item_m, NULL)
    bifactor_report(item_m[keep, , drop = FALSE],
                    body$item$exercise_id, colnames(item_m))
  }, error = function(e) { res$status <- 500
    list(error = paste("bifactor failed:", conditionMessage(e))) })
}

#* Mokken nonparametric scalability (Loevinger H, AISP)
#* @post /mokken
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item)) {
    res$status <- 400; return(list(error = "Need item block."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    keep   <- clean_matrix(item_m, NULL)
    mokken_report(item_m[keep, , drop = FALSE], colnames(item_m))
  }, error = function(e) { res$status <- 500
    list(error = paste("mokken failed:", conditionMessage(e))) })
}

#* Mantel-Haenszel DIF across a two-level grouping variable
#* @post /dif_mh
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item) || is.null(body$group)) {
    res$status <- 400
    return(list(error = "Need item block and a group vector (length = n persons)."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    cfg    <- merge_config(DEFAULTS, body$config)
    mh_dif_report(item_m, body$group, n_strata = cfg$mh_strata %||% 4)
  }, error = function(e) { res$status <- 500
    list(error = paste("MH DIF failed:", conditionMessage(e))) })
}

#* Health check
#* @get /
function() {
  list(status = "ok", service = "DLTPT IRT", version = "2.3",
       engines = c("irt", "form", "bifactor", "mokken", "dif_mh"))
}


# =====================================================================
# IRT Analysis API for DLTPT  --  ADDITIONS v2.1
# =====================================================================
# Drop-in extensions to the v2.0 plumber service. Everything here reuses
# the existing shared helpers (extract_item_params, wle_summary, fit_*,
# merge_config, DEFAULTS) and the COMMON OUTPUT CONTRACT from run_method().
# Nothing below removes or rewrites an existing function.
#
# WHY THESE FIVE. This service is the measurement backend for a C-Test
# validation programme whose lead study is the standard-setting GRAIN-SIZE
# question: does a holistic passage-level (extended-Angoff) cut score
# reproduce the item-by-item gap-level Angoff cut, and classify students
# into CEFR levels as reliably? (Eckes & Baghaei 2015 established that
# within-passage gaps are locally dependent, so the passage is the honest
# unit; Alpizar et al. 2022 mapped the dichotomous/polytomous/testlet
# trade-off; Park 2022 is the precedent for substituting a coarser
# judgment unit.) The v2.0 /form engine fits the three scoring models and
# shows they *rank* students almost identically -- but that concordance is
# largely mechanical (all three thetas are near-monotone in the raw score),
# it never quantifies the local dependence that motivates the whole paper,
# and it has no bridge from an IRT theta to a defensible cut score. These
# five additions close exactly those gaps.
#
#   1. ld_by_group()          Within- vs between-passage Yen's Q3. Turns
#                             "reliabilities differ" into the *reason* they
#                             differ. This is the paper's rationale, computed.
#   2. model_comparison()     AIC/BIC across 2PL/Rasch/PCM. Answers "is the
#                             richer model earning its parameters" (at DLTPT
#                             N it usually is not -> Rasch/PCM preferred).
#   3. TCC linking            theta <-> expected-raw-score bridge. Required to
#                             move any raw Angoff cut onto the theta scale and
#                             back. Pure IRT identity, no new dependency.
#   4. standard-setting core  gap_level_angoff / passage_level_angoff /
#                             grain_agreement / decision_consistency. THE
#                             paper's engine: two cut scores, their theta
#                             gap, reclassification %, and Rudner-style
#                             classification accuracy + decision consistency.
#   5. gtheory_angoff()       Judge x unit variance decomposition + G/Phi.
#                             The "reproducibility, not just agreement" half:
#                             does collapsing 100 gap judgments into 5 passage
#                             judgments change the cut-score error structure?
#
# All five have offline-testable cores; two new routes (/ld, /standard_set)
# and three additions to run_form_analysis() wire them into the service.
# Validated end-to-end on the DLTPT form (5 exercises x 20 gaps, n=111).
# =====================================================================


# =====================================================================
# ADDITION 1 -- LOCAL DEPENDENCE BY GROUP (within vs between passage)
# =====================================================================
# The v2.0 /irt engine already computes Yen's Q3 within a single exercise.
# For a C-Test FORM the diagnostic that matters is comparative: are gaps
# more dependent WITHIN their own passage than BETWEEN passages? A positive
# gap is the signature of testlet structure and the direct justification
# for exercise-scope (PCM) scoring over item-scope. `group` is the passage
# id per item (body$item$exercise_id in the /form payload).
ld_by_group <- function(item_mod, item_names, group,
                        q3_threshold = 0.2, top_n = 10) {
  mf <- tryCatch(suppressWarnings(TAM::tam.modelfit(item_mod, progress = FALSE)),
                 error = function(e) NULL)
  if (is.null(mf) || is.null(mf$Q3.matr))
    return(list(available = FALSE, note = "Q3 unavailable for this model."))

  q3  <- mf$Q3.matr
  nm  <- colnames(q3)
  grp <- setNames(as.character(group), item_names)[nm]   # align to Q3 col order
  ut  <- upper.tri(q3)
  idx <- which(ut, arr.ind = TRUE)
  vals<- q3[ut]
  same<- grp[idx[, 1]] == grp[idx[, 2]]

  summ <- function(v) list(
    n_pairs     = length(v),
    mean_q3     = round(mean(v, na.rm = TRUE), 4),
    mean_abs_q3 = round(mean(abs(v), na.rm = TRUE), 4),
    n_flagged   = sum(abs(v) > q3_threshold, na.rm = TRUE),
    pct_flagged = round(100 * mean(abs(v) > q3_threshold, na.rm = TRUE), 1)
  )
  w  <- which(same)[order(-abs(vals[same]))]
  w  <- head(w, top_n)
  top_within <- data.frame(
    Item1 = nm[idx[w, 1]], Item2 = nm[idx[w, 2]],
    Q3 = round(vals[w], 3), stringsAsFactors = FALSE)

  within <- summ(vals[same]); between <- summ(vals[!same])
  list(
    available        = TRUE,
    q3_threshold     = q3_threshold,
    within_passage   = within,
    between_passage  = between,
    ld_gap           = round(within$mean_abs_q3 - between$mean_abs_q3, 4),
    madaq3           = round(mean(abs(vals - mean(vals, na.rm = TRUE)), na.rm = TRUE), 4),
    top_within_pairs = top_within,
    interpretation   = if (within$mean_abs_q3 > between$mean_abs_q3)
        paste("Within-passage residual dependence exceeds between-passage:",
              "item-scope (2PL/Rasch) reliability is optimistic; prefer",
              "exercise-scope (PCM), whose super-item absorbs the dependence.")
      else
        "No excess within-passage dependence detected; item-scope scoring defensible."
  )
}


# =====================================================================
# ADDITION 2 -- MODEL COMPARISON (fit indices across scoring models)
# =====================================================================
# /form fits 2PL, Rasch and PCM but never says which is statistically
# preferable. At C-Test-typical N the 2PL's per-item discriminations
# frequently fail to earn their parameters; BIC is the honest arbiter.
# Returns one row per successfully-fitted method.
model_comparison <- function(fits, scopes = NULL) {
  rows <- lapply(names(fits), function(nm) {
    m  <- fits[[nm]]
    ic <- tryCatch(m$ic, error = function(e) NULL)
    data.frame(
      method = nm,
      scope  = if (!is.null(scopes)) scopes[[nm]] %||% NA_character_ else NA_character_,
      npars  = tryCatch(ic$Npars, error = function(e) NA_real_),
      n_obs  = tryCatch(ic$n,     error = function(e) NA_real_),
      loglik = tryCatch(round(as.numeric(logLik(m)), 1), error = function(e) NA_real_),
      AIC    = tryCatch(round(ic$AIC, 1), error = function(e) NA_real_),
      BIC    = tryCatch(round(ic$BIC, 1), error = function(e) NA_real_),
      stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)

  tab$comparable       <- TRUE
  tab$preferred_by_BIC <- FALSE
  if (!is.null(scopes) && length(unique(stats::na.omit(tab$scope))) > 1) {
    sc_counts  <- sort(table(tab$scope), decreasing = TRUE)
    main_scope <- names(sc_counts)[1]           # largest same-scope set
    tab$comparable <- tab$scope == main_scope
  }
  cand <- tab[tab$comparable & !is.na(tab$BIC), , drop = FALSE]
  if (nrow(cand) > 0)
    tab$preferred_by_BIC <- tab$method == cand$method[which.min(cand$BIC)]

  attr(tab, "note") <- paste(
    "AIC/BIC compared only within a single response scope (same observations",
    "and likelihood scale). Cross-scope rows (e.g. exercise-level PCM vs",
    "item-level 2PL/Rasch) are shown for reference, marked comparable=FALSE,",
    "and excluded from the preferred-model choice.")
  tab
}


# =====================================================================
# ADDITION 3 -- TCC LINKING (theta <-> expected raw score)
# =====================================================================
# The bridge every raw-scale cut score needs to reach the theta metric and
# back. For a dichotomous item-scope model the Test Characteristic Curve is
# the sum of item success probabilities; it is monotone in theta, so the
# inverse is a simple grid search. Pure 2PL/Rasch identity -- no new dep.
tcc_raw_from_theta <- function(a, b, theta) {
  vapply(theta, function(t) sum(1 / (1 + exp(-a * (t - b)))), numeric(1))
}
theta_from_raw <- function(a, b, raw_cut, grid = seq(-6, 6, 0.001)) {
  tcc <- tcc_raw_from_theta(a, b, grid)
  grid[which.min(abs(tcc - raw_cut))]
}


# =====================================================================
# ADDITION 4 -- STANDARD-SETTING GRAIN-SIZE ENGINE
# =====================================================================
# The lead study, computed. Inputs are borderline-student judgment matrices:
#   gap-level:     judges x gaps      cell = P(borderline answers gap right)
#   passage-level: judges x passages  cell = expected passage score (0..max)
# Both produce a raw cut = sum of cells, averaged over judges. grain_agreement
# links each raw cut to theta via the TCC and reports whether the two grains
# classify students the same way. decision_consistency is the Rudner-style
# accuracy/consistency of a theta cut given per-person WLE theta + SE.

gap_level_angoff <- function(gap_prob_matrix) {
  per_judge <- rowSums(gap_prob_matrix)
  list(cut_raw   = mean(per_judge),
       sd_judges = sd(per_judge),
       per_judge = as.numeric(per_judge),
       n_judges  = nrow(gap_prob_matrix),
       n_units   = ncol(gap_prob_matrix))
}

passage_level_angoff <- function(passage_score_matrix) {
  per_judge <- rowSums(passage_score_matrix)
  list(cut_raw   = mean(per_judge),
       sd_judges = sd(per_judge),
       per_judge = as.numeric(per_judge),
       n_judges  = nrow(passage_score_matrix),
       n_units   = ncol(passage_score_matrix))
}

# Rudner-style classification accuracy & decision consistency at a theta cut.
decision_consistency <- function(theta, se, theta_cut) {
  ok <- !is.na(theta) & !is.na(se) & se > 0
  th <- theta[ok]; s <- se[ok]
  if (length(th) == 0) return(list(n = 0L, note = "no usable theta/SE"))
  p_above   <- 1 - pnorm(theta_cut, mean = th, sd = s)
  obs_above <- th >= theta_cut
  acc  <- mean(ifelse(obs_above, p_above, 1 - p_above))
  cons <- mean(p_above^2 + (1 - p_above)^2)
  list(n = sum(ok), theta_cut = round(theta_cut, 3),
       pct_above = round(100 * mean(obs_above), 1),
       classification_accuracy = round(acc, 3),
       decision_consistency    = round(cons, 3))
}

# Compare two raw cut scores (gap vs passage grain): theta gap + reclassification.
grain_agreement <- function(raw_cut_gap, raw_cut_pass, a, b, theta, se = NULL) {
  tcut_gap  <- theta_from_raw(a, b, raw_cut_gap)
  tcut_pass <- theta_from_raw(a, b, raw_cut_pass)
  cls_g <- as.integer(theta >= tcut_gap)
  cls_p <- as.integer(theta >= tcut_pass)
  tab <- table(factor(cls_g, 0:1), factor(cls_p, 0:1)); n <- sum(tab)
  po  <- sum(diag(tab)) / n
  pe  <- sum(rowSums(tab) * colSums(tab)) / n^2
  kappa <- if (pe == 1) NA_real_ else (po - pe) / (1 - pe)
  out <- list(
    raw_cut_gap      = round(raw_cut_gap, 2),
    raw_cut_pass     = round(raw_cut_pass, 2),
    theta_cut_gap    = round(tcut_gap, 3),
    theta_cut_pass   = round(tcut_pass, 3),
    theta_cut_diff   = round(tcut_pass - tcut_gap, 3),
    pct_reclassified = round(100 * mean(cls_g != cls_p), 1),
    classification_kappa = round(kappa, 3))
  if (!is.null(se)) out$decision_consistency <- list(
    gap     = decision_consistency(theta, se, tcut_gap),
    passage = decision_consistency(theta, se, tcut_pass))
  out
}


# =====================================================================
# ADDITION 5 -- G-THEORY DECOMPOSITION OF ANGOFF RATINGS
# =====================================================================
# "Reproducibility, not just agreement." A judges x units rating matrix is
# decomposed (random two-way, judge x unit) into variance components. A
# defensible standard has most variance in UNITS (real difficulty spread)
# and little in JUDGES (disagreement about the border). Collapsing 100 gaps
# into 5 passages changes this structure; only a G-study shows whether the
# cut-score error is neutral, better, or worse at the coarse grain
# (Clauser et al. 2002). G_relative is the generalizability coefficient.
gtheory_angoff <- function(rating_matrix) {
  nj <- nrow(rating_matrix); ni <- ncol(rating_matrix)
  if (nj < 2 || ni < 2) return(list(error = "need >=2 judges and >=2 units"))
  gm <- mean(rating_matrix)
  jm <- rowMeans(rating_matrix); im <- colMeans(rating_matrix)
  SS_j  <- ni * sum((jm - gm)^2)
  SS_i  <- nj * sum((im - gm)^2)
  SS_t  <- sum((rating_matrix - gm)^2)
  SS_ji <- SS_t - SS_j - SS_i
  MS_j  <- SS_j  / (nj - 1)
  MS_i  <- SS_i  / (ni - 1)
  MS_ji <- SS_ji / ((nj - 1) * (ni - 1))
  v_j  <- max((MS_j  - MS_ji) / ni, 0)   # judge (unwanted)
  v_i  <- max((MS_i  - MS_ji) / nj, 0)   # unit  (wanted: real difficulty spread)
  v_ji <- max(MS_ji, 0)                   # residual
  abs_err_var <- v_j / nj + v_ji / (nj * ni)
  G   <- if (v_i + v_ji / nj > 0) v_i / (v_i + v_ji / nj) else NA_real_
  Phi <- if (v_i + abs_err_var > 0) v_i / (v_i + abs_err_var) else NA_real_
  list(
    n_judges = nj, n_units = ni,
    var_judge = round(v_j, 4), var_unit = round(v_i, 4), var_resid = round(v_ji, 4),
    pct_judge = round(100 * v_j / (v_j + v_i + v_ji), 1),
    G_relative = round(G, 3), Phi_absolute = round(Phi, 3),
    se_cut_per_unit = round(sqrt(abs_err_var), 4),
    interpretation = if (!is.na(G) && G < 0.8)
        "Low G: the cut score is judge-dependent; add judges or discussion rounds."
      else "High G: judges reproduce the standard; cut score is dependable at this grain."
  )
}


# =====================================================================
# NEW ROUTE -- /standard_set  (grain-size standard setting)
# =====================================================================
# Offline-testable core. Payload:
#   { item:{columns, exercise_id, rows},        # for the TCC (dichotomous)
#     ratings:{
#       gap:     [[judge1 gap probs...], ...],   # nJudge x nGap  in [0,1]
#       passage: [[judge1 passage exp...], ...]  # nJudge x nPassage in 0..max
#     },
#     model, config }
# Returns both cut scores, their theta linking + reclassification, decision
# consistency at each cut, and a G-study for each grain.
run_standard_setting <- function(body) {
  cfg <- merge_config(DEFAULTS, body$config)
  if (is.null(body$item) || is.null(body$ratings))
    return(list(error = "Need item block (for TCC) and ratings{gap,passage}.",
                status_code = 400))

  item_m <- block_to_matrix(body$item)
  keep   <- clean_matrix(item_m, NULL)
  resp_c <- item_m[keep, , drop = FALSE]
  model  <- tolower(body$model %||% "rasch")
  mod    <- if (model == "2pl") fit_2pl(resp_c) else fit_rasch(resp_c)
  if (inherits(mod, "tam_error"))
    return(list(error = paste("TCC model failed:", mod$error), status_code = 500))
  p   <- extract_item_params(mod, if (model == "2pl") "2pl" else "rasch")
  wle <- wle_summary(mod)

  to_m <- function(x) if (is.matrix(x)) x else do.call(rbind, x)
  out  <- list(model = toupper(model), n_calib = nrow(resp_c), max_raw = ncol(resp_c))

  if (!is.null(body$ratings$gap) && !is.null(body$ratings$passage)) {
    gl <- gap_level_angoff(to_m(body$ratings$gap))
    pl <- passage_level_angoff(to_m(body$ratings$passage))
    out$gap_level     <- gl[c("cut_raw", "sd_judges", "n_judges", "n_units")]
    out$passage_level <- pl[c("cut_raw", "sd_judges", "n_judges", "n_units")]
    out$grain_agreement <- grain_agreement(gl$cut_raw, pl$cut_raw,
                                            p$a, p$b, wle$theta, wle$se)
    out$gtheory <- list(gap     = gtheory_angoff(to_m(body$ratings$gap)),
                        passage = gtheory_angoff(to_m(body$ratings$passage)))
  }
  out$tcc <- list(theta = seq(-4, 4, 0.1),
                  expected_raw = round(tcc_raw_from_theta(p$a, p$b, seq(-4, 4, 0.1)), 2))
  out$config_used <- cfg
  out
}

#* Grain-size standard setting (gap-level vs passage-level Angoff)
#* @post /standard_set
#* @post /standard_set/
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body)) { res$status <- 400; return(list(error = "Invalid JSON in request body.")) }
  tryCatch({
    out <- run_standard_setting(body)
    if (!is.null(out$error)) { res$status <- out$status_code %||% 400; return(list(error = out$error)) }
    out
  }, error = function(e) { res$status <- 500
    list(error = paste("standard setting failed:", conditionMessage(e))) })
}

#* Local-dependence report for a form (within vs between passage Q3)
#* @post /ld
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item)) {
    res$status <- 400; return(list(error = "Need item block with exercise_id."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    keep   <- clean_matrix(item_m, NULL)
    mod    <- fit_rasch(item_m[keep, , drop = FALSE])
    if (inherits(mod, "tam_error")) { res$status <- 500; return(list(error = mod$error)) }
    cfg <- merge_config(DEFAULTS, body$config)
    ld_by_group(mod, colnames(item_m), body$item$exercise_id, cfg$q3_threshold)
  }, error = function(e) { res$status <- 500
    list(error = paste("LD analysis failed:", conditionMessage(e))) })
}


# =====================================================================
# ADDITION 6 -- DIMENSIONALITY (construct validity, the review's core debate)
# =====================================================================
# The review's longest-running controversy: one dimension or several?
# PCA of Rasch standardized residuals (Winsteps-style). A large first
# residual eigenvalue (>~2 in item units) signals a secondary dimension;
# for a C-Test the expected shape is passage-specific factors (Eckes &
# Grotjahn 2006). The tool reports the contrast AND ties it to USE: the
# variance is only a problem if it changes decisions (the research-gap
# map's reframing of dimensionality as a consequential-validity question).
dimensionality_report <- function(item_mod, item_names, group) {
  res <- tryCatch(IRT.residuals(item_mod), error = function(e) NULL)
  if (is.null(res) || is.null(res$stand_residuals))
    return(list(available = FALSE, note = "standardized residuals unavailable"))
  stdres <- res$stand_residuals
  stdres <- stdres[, apply(stdres, 2, function(x) sd(x, na.rm = TRUE) > 0), drop = FALSE]
  cc <- cor(stdres, use = "pairwise.complete.obs"); cc[is.na(cc)] <- 0
  ev <- eigen(cc, only.values = TRUE)$values
  k  <- ncol(stdres)
  grp <- setNames(as.character(group), item_names)[colnames(stdres)]
  pass_res <- t(apply(stdres, 1, function(r) tapply(r, grp, mean, na.rm = TRUE)))
  list(
    available = TRUE, n_items_used = k,
    first_residual_eigenvalue = round(ev[1], 3),
    pct_first_contrast = round(100 * ev[1] / k, 2),
    eigenvalues_top5 = round(ev[1:min(5, length(ev))], 3),
    passage_residual_var = round(mean(apply(pass_res, 2, var, na.rm = TRUE)), 4),
    rule_of_thumb = "First residual eigenvalue >~2 item-units suggests a secondary dimension.",
    interpretation = if (ev[1] > 2)
        paste("Secondary dimension present; consistent with passage-specific factors",
              "(Eckes & Grotjahn 2006). A single global score is defensible only if this",
              "variance is decision-inconsequential -- test with grain_agreement / DIF.")
      else "No strong secondary dimension; unidimensional person ordering supported."
  )
}


# =====================================================================
# ADDITION 7 -- DIF / FAIRNESS (the review's highest-value outstanding gap)
# =====================================================================
# "Fairness evidence is disproportionately small relative to the stakes."
# ETS-style Rasch difficulty contrast across a two-level grouping (L1,
# gender, content background), SE-STANDARDIZED as a Wald z -- a raw-logit
# threshold over-flags at the small per-group N typical of C-Test studies.
# Items lacking response variance in either subgroup are screened out
# first (they drive difficulty to the estimator edge). The review
# recommends TESTLET-level DIF under within-passage dependence: pass the
# 5-passage sum-score matrix with a polytomous fit for that variant.
# NOTE: at small per-group N item-level DIF is underpowered; the note field
# says so and points to the passage-level alternative.
dif_report <- function(resp, group_vec, dif_threshold = 0.5,
                       min_var_p = 0.05, z_crit = 1.96) {
  g <- as.factor(group_vec); levs <- levels(g)
  if (length(levs) != 2) return(list(available = FALSE, note = "DIF needs exactly 2 groups"))
  r1 <- resp[g == levs[1], , drop = FALSE]
  r2 <- resp[g == levs[2], , drop = FALSE]
  pc1 <- colMeans(r1, na.rm = TRUE); pc2 <- colMeans(r2, na.rm = TRUE)
  usable  <- names(pc1)[pc1 > min_var_p & pc1 < 1 - min_var_p &
                        pc2 > min_var_p & pc2 < 1 - min_var_p]
  dropped <- setdiff(colnames(resp), usable)
  if (length(usable) < 3) return(list(available = FALSE,
    note = "too few items with adequate variance in both groups"))
  fit_bse <- function(mat) {
    m  <- suppressMessages(tam.mml(mat[, usable, drop = FALSE], verbose = FALSE))
    b  <- extract_item_params(m, "rasch")$b; names(b) <- usable
    se <- setNames(m$xsi$se.xsi[seq_along(usable)], usable)
    list(b = b - mean(b), se = se)
  }
  f1 <- fit_bse(r1); f2 <- fit_bse(r2)
  delta <- f1$b - f2$b
  se_d  <- sqrt(f1$se^2 + f2$se^2)
  z     <- delta / se_d
  tab <- data.frame(
    Item = names(delta), DIF = round(delta, 3), SE = round(se_d, 3),
    z = round(z, 2),
    flag = ifelse(abs(z) > z_crit & abs(delta) > dif_threshold, "DIF", ""),
    stringsAsFactors = FALSE)
  tab <- tab[order(-abs(tab$z)), ]
  list(
    available = TRUE, groups = levs,
    method = "ETS Rasch difficulty contrast, SE-standardized (Wald z)",
    n_items_tested = length(usable), n_items_dropped = length(dropped),
    dropped_items = dropped,
    n_flagged = sum(tab$flag == "DIF"), max_abs_z = round(max(abs(z)), 2),
    items = tab,
    note = paste("Flag = |DIF|>0.5 logit AND |z|>1.96. At small per-group N item-level",
                 "DIF is underpowered; testlet-level DIF (5 passage super-items) is more",
                 "powerful and respects within-passage dependence (Eckes & Baghaei 2015).")
  )
}

#* Dimensionality report (PCA of Rasch residuals) for a form
#* @post /dimensionality
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item)) {
    res$status <- 400; return(list(error = "Need item block with exercise_id."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    mod <- fit_rasch(item_m[clean_matrix(item_m, NULL), , drop = FALSE])
    if (inherits(mod, "tam_error")) { res$status <- 500; return(list(error = mod$error)) }
    dimensionality_report(mod, colnames(item_m), body$item$exercise_id)
  }, error = function(e) { res$status <- 500
    list(error = paste("dimensionality failed:", conditionMessage(e))) })
}

#* DIF / fairness report across a two-level grouping variable
#* @post /dif
#* @serializer json
function(req, res) {
  body <- parse_body(req)
  if (is.null(body) || is.null(body$item) || is.null(body$group)) {
    res$status <- 400; return(list(error = "Need item block and a group vector (length = n persons)."))
  }
  tryCatch({
    item_m <- block_to_matrix(body$item)
    cfg <- merge_config(DEFAULTS, body$config)
    dif_report(item_m, body$group,
               dif_threshold = cfg$dif_threshold %||% 0.5)
  }, error = function(e) { res$status <- 500
    list(error = paste("DIF failed:", conditionMessage(e))) })
}
