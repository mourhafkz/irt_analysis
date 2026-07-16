#
# IRT Analysis API for DLTPT  --  v2.0
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
# =====================================================================

library(plumber)
library(jsonlite)
library(TAM)


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
  if (!is.null(requested) && nzchar(requested)) {
    m <- tolower(requested)
    if (m %in% c("rasch", "1pl")) return(list(model = "rasch", selection = "explicit"))
    if (m %in% c("2pl"))          return(list(model = "2pl",   selection = "explicit"))
    return(list(model = NA, selection = "invalid"))
  }
  if (n_cal >= cfg$twopl_min_n) list(model = "2pl",   selection = "auto")
  else                         list(model = "rasch", selection = "auto")
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
run_method <- function(method, item_m, exercise_m, exercise_max, person_keys, cfg) {
  if (method %in% c("item_2pl", "item_rasch")) {
    resp <- item_m; max_scores <- NULL; scope <- "item"
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
    model  = switch(method, item_2pl = "2PL", item_rasch = "Rasch", exercise_pcm = "PCM"),
    n_used = nrow(resp_c),
    person_keys_used = pk_c,
    theta = as.list(theta), se = as.list(se),
    reliability = list(
      eap = eap,
      wle = if (!is.na(wle$reliability)) round(wle$reliability, 3) else NA_real_
    ),
    items = it$items, item_order = it$order,
    .theta = theta   # out-of-band for Layer C; dropped before serialize
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
  exercise_m   <- if (!is.null(body$exercise)) block_to_matrix(body$exercise)      else NULL
  exercise_max <- if (!is.null(body$exercise)) as.numeric(body$exercise$max_scores) else NULL
  # body$item$exercise_id reserved for bifactor (later method); unused here.

  results <- lapply(methods, function(m)
    run_method(m, item_m, exercise_m, exercise_max, person_keys, cfg))

  comparison <- layer_c(results, cfg)
  results <- lapply(results, function(r) { r$.theta <- NULL; r })

  list(
    methods_requested = methods,
    results           = results,
    comparison        = comparison,
    coverage          = body$coverage,
    exercises         = body$exercises,
    config_used       = cfg
  )
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

#* Health check
#* @get /
function() {
  list(status = "ok", service = "DLTPT IRT", version = "2.0",
       engines = c("irt", "form"))
}
