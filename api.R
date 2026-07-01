#
# IRT Analysis API for DLTPT
# 2PL model via TAM, with teacher-facing interpretations
#

library(plumber)
library(jsonlite)
library(TAM)


# =========================================================================
# Helpers (defined once, used by the route)
# =========================================================================

# ---- Safe JSON parsing ---------------------------------------------------
parse_body <- function(req) {
  tryCatch(
    fromJSON(req$postBody),
    error = function(e) NULL
  )
}

# ---- Matrix -> data.frame ------------------------------------------------
matrix_to_df <- function(matrix_obj) {
  tryCatch({
    rows_list <- if (is.list(matrix_obj$rows[[1]])) {
      matrix_obj$rows
    } else {
      list(matrix_obj$rows)
    }
    mat <- do.call(rbind, rows_list)
    df  <- as.data.frame(mat)
    colnames(df) <- matrix_obj$columns
    df
  }, error = function(e) NULL)
}

# ---- Strip person/student ID column(s) regardless of position ------------
strip_id_columns <- function(df) {
  id_names <- c("person_id", "student_id", "id", "user_id")
  id_idx   <- which(tolower(colnames(df)) %in% id_names)
  if (length(id_idx) > 0) df[, -id_idx, drop = FALSE] else df
}

# ---- Fit the 2PL model ---------------------------------------------------
fit_2pl <- function(resp) {
  tryCatch(
    suppressMessages(tam.mml.2pl(resp, verbose = FALSE)),
    error = function(e) structure(list(error = e$message), class = "tam_error")
  )
}

# ---- Response diagnostics: who got dropped and why -----------------------
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

# ---- NA-safe logical mask ------------------------------------------------
# Undefined item statistics (e.g. a constant item's item-total correlation
# comes back as NA) must never enter a `[<-` subscript, or R errors with
# "NAs are not allowed in subscripted assignments". Coerce NA -> FALSE.
na_false <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

# ---- Item-level flags (two tiers: broken vs. weak) -----------------------
build_item_flags <- function(resp, mod) {
  discrimination <- mod$B[, , 1][, 2]
  # FIX #1: TAM's 2PL is P = invlogit(a*theta - xsi), so xsi is the intercept.
  # The classical difficulty (theta where P = 0.5) is b = xsi / a. Using xsi
  # directly is only correct when a = 1 (Rasch); on a 2PL it is off by a factor
  # of a per item -- worst on the high-slope items.
  difficulty     <- mod$xsi$xsi / discrimination
  prop_correct   <- colMeans(resp, na.rm = TRUE)

  total_score    <- rowSums(resp, na.rm = TRUE)
  item_total_cor <- sapply(
    resp,
    function(x) {
      # A constant item (all-correct / all-wrong) has zero SD; cor() then
      # returns NA and warns. That's expected here -- suppress the noise.
      suppressWarnings(cor(x, total_score - x, use = "pairwise.complete.obs"))
    }
  )

  fit    <- tam.fit(mod)$itemfit
  outfit <- fit$Outfit
  infit  <- fit$Infit

  flags <- data.frame(
    Item           = colnames(resp),
    Difficulty     = round(difficulty,     3),
    Discrimination = round(discrimination, 3),
    PropCorrect    = round(prop_correct,   3),
    ItemTotalCor   = round(item_total_cor, 3),
    Infit          = round(infit,          3),
    Outfit         = round(outfit,         3),
    stringsAsFactors = FALSE
  )

  # Two-tier flagging. Every mask is wrapped in na_false() so items with
  # undefined statistics don't break the subscripted assignments below.
  flags$Flag <- ""

  # Severe: the item is probably broken
  broken_mask <- na_false(flags$Discrimination < 0 | flags$ItemTotalCor < 0)
  flags$Flag[broken_mask] <- paste(flags$Flag[broken_mask], "BROKEN_negDisc")

  # Weak but not broken
  weak_disc <- na_false(flags$Discrimination >= 0 & flags$Discrimination < 0.6)
  flags$Flag[weak_disc] <- paste(flags$Flag[weak_disc], "WeakDisc")

  # FIX #3: Degenerate-high discrimination. The slope ran to the estimator's
  # edge. With small N this is a near-Guttman column, not a superb item -- it
  # inflates information (hence reliability) and is unstable across samples.
  over_disc <- na_false(flags$Discrimination > 3)
  flags$Flag[over_disc] <- paste(flags$Flag[over_disc], "UnstableDisc")

  too_easy <- na_false(flags$PropCorrect > 0.95)
  flags$Flag[too_easy] <- paste(flags$Flag[too_easy], "TooEasy")

  too_hard <- na_false(flags$PropCorrect < 0.20)
  flags$Flag[too_hard] <- paste(flags$Flag[too_hard], "TooHard")

  low_corr <- na_false(flags$ItemTotalCor < 0.30 & flags$ItemTotalCor >= 0)
  flags$Flag[low_corr] <- paste(flags$Flag[low_corr], "LowCorr")

  infit_high <- na_false(flags$Infit > 1.3)
  flags$Flag[infit_high] <- paste(flags$Flag[infit_high], "InfitHigh")

  outfit_high <- na_false(flags$Outfit > 2.0)
  flags$Flag[outfit_high] <- paste(flags$Flag[outfit_high], "OutfitHigh")

  flags$Flag <- trimws(flags$Flag)
  flags
}

# ---- Yen's Q3 (residual correlations) for local dependence ---------------
compute_q3 <- function(mod, threshold = 0.2) {
  q3_result <- tryCatch(
    suppressWarnings(tam.modelfit(mod, progress = FALSE)),
    error = function(e) NULL
  )
  if (is.null(q3_result) || is.null(q3_result$Q3.matr)) {
    return(data.frame(
      Item1 = character(0), Item2 = character(0),
      Q3 = numeric(0), Recommendation = character(0)
    ))
  }

  q3_mat   <- q3_result$Q3.matr
  upper    <- which(abs(q3_mat) > threshold & upper.tri(q3_mat), arr.ind = TRUE)
  if (nrow(upper) == 0) {
    return(data.frame(
      Item1 = character(0), Item2 = character(0),
      Q3 = numeric(0), Recommendation = character(0)
    ))
  }

  data.frame(
    Item1          = colnames(q3_mat)[upper[, 1]],
    Item2          = colnames(q3_mat)[upper[, 2]],
    Q3             = round(q3_mat[upper], 3),
    Recommendation = "Items may share construct-irrelevant variance; consider revising one.",
    stringsAsFactors = FALSE
  )
}

# ---- WLE summary ---------------------------------------------------------
wle_summary <- function(mod) {
  res    <- tam.wle(mod, progress = FALSE)
  theta  <- res$theta
  se     <- res$error
  m      <- mean(theta, na.rm = TRUE)
  s      <- sd(theta,   na.rm = TRUE)
  v      <- var(theta,  na.rm = TRUE)
  # Guard against pathological values
  rel <- if (!is.na(v) && v > 0) 1 - mean(se^2, na.rm = TRUE) / v else NA_real_
  rel <- if (!is.na(rel) && rel < 0) 0 else rel
  list(mean = m, sd = s, reliability = rel)
}

# ---- Build ICC curves for the frontend -----------------------------------
build_icc <- function(mod, item_names, theta_grid = seq(-4, 4, length.out = 100)) {
  lapply(seq_along(item_names), function(i) {
    a <- mod$B[i, , 1][2]
    # FIX #1 (same parameterization issue as build_item_flags): the P = 0.5
    # location is xsi / a, not xsi. Without this the curve is shifted by a
    # factor of a -- e.g. ~5x for an item with a ~ 5.
    b <- mod$xsi$xsi[i] / a
    P <- 1 / (1 + exp(-a * (theta_grid - b)))
    list(
      item        = item_names[i],
      theta       = theta_grid,
      probability = round(P, 4)
    )
  })
}

# =========================================================================
# Teacher-facing text generators
# =========================================================================

text_eap <- function(r) {
  if (is.na(r))           "EAP reliability could not be estimated from this sample."
  else if (r < 0.7)       "EAP reliability is below 0.7. The test cannot reliably separate students."
  else if (r < 0.8)       "EAP reliability is close to 0.8, indicating acceptable reliability."
  else if (r < 0.9)       "EAP reliability is above 0.8, indicating good reliability."
  else                    "EAP reliability is 0.9 or above, indicating excellent reliability."
}

text_wle_rel <- function(r) {
  if (is.na(r))           "WLE reliability could not be estimated from this sample."
  else if (r < 0.7)       "WLE reliability is below 0.7, indicating low reliability."
  else if (r < 0.8)       "WLE reliability is close to 0.8, indicating acceptable reliability."
  else if (r < 0.9)       "WLE reliability is above 0.8, indicating good reliability."
  else                    "WLE reliability is 0.9 or above, indicating excellent reliability."
}

text_mean <- function(m) {
  if (is.na(m))           "Mean student ability could not be estimated."
  else if (m >  1)        "Mean ability is above 1: the test appears easy for this group."
  else if (m < -1)        "Mean ability is below -1: the test appears difficult for this group."
  else                    "Mean ability is close to 0: the test is suitable for most students."
}

text_sd <- function(s) {
  if (is.na(s))           "Ability spread could not be estimated."
  else if (s < 0.5)       "Abilities are very similar; the test does not differentiate students well."
  else if (s < 0.8)       "Abilities show some variation, but differences are still small."
  else if (s < 1.3)       "Healthy spread in abilities; the test differentiates well between students."
  else                    "Abilities vary substantially, indicating large performance differences."
}

text_ability <- function(m) {
  if (is.na(m))           "Cannot determine target ability level from this sample."
  else if (m >  1)        "The exercise targets high-ability students."
  else if (m < -0.5)      "The exercise targets low-ability students."
  else                    "The exercise targets average-ability students."
}

text_variance <- function(s) {
  if (!is.na(s) && s < 0.5)
    "Consider adding items that vary more in difficulty to better distinguish students."
  else
    ""
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
  parts <- c(
    text_ability(mean_wle),
    text_wle_rel(wle_rel),
    text_variance(sd_wle),
    text_improvement(wle_rel)
  )
  paste(parts[nzchar(parts)], collapse = "\n")
}


# =========================================================================
# API
# =========================================================================

#* @apiTitle IRT Analysis for DLTPT
#* @apiDescription 2PL IRT analysis, item diagnostics, and teacher-facing interpretations.

#* Run 2PL IRT analysis on a response matrix
#* @post /irt
#* @post /irt/
#* @serializer json
function(req, res) {

  # ---- Parse and validate ----
  body <- parse_body(req)
  if (is.null(body)) {
    res$status <- 400
    return(list(error = "Invalid JSON in request body."))
  }

  if (is.null(body$matrix$rows) || length(body$matrix$rows) == 0) {
    res$status <- 400
    return(list(error = "Empty matrix in payload."))
  }

  df <- matrix_to_df(body$matrix)
  if (is.null(df)) {
    res$status <- 400
    return(list(error = "Could not convert matrix to data.frame."))
  }

  resp <- strip_id_columns(df)

  if (ncol(resp) < 3) {
    res$status <- 400
    return(list(error = "Too few items for IRT analysis (need at least 3)."))
  }
  if (nrow(resp) < 20) {
    res$status <- 400
    return(list(error = "Too few respondents for stable 2PL estimation (need at least 20)."))
  }

  # Coerce to numeric matrix (guard against character columns from JSON)
  resp[] <- lapply(resp, function(x) as.numeric(as.character(x)))

  # ---- Diagnostics: report on the matrix AS RECEIVED ----
  diag_info <- response_diagnostics(resp)

  # FIX #2: Drop perfect / zero / incomplete BEFORE calibration.
  # response_diagnostics only *counts* these; previously the model was still fit
  # on the full matrix, so perfect/zero rows pulled the MML item parameters (and
  # thus reliability) while the diagnostics claimed they were excluded. Fit on
  # the kept rows so the model matches what n_used_for_calib reports. Uses the
  # exact same predicate as response_diagnostics().
  total    <- rowSums(resp, na.rm = TRUE)
  keep_row <- !(total == 0 | total == ncol(resp) | rowSums(is.na(resp)) > 0)
  resp_cal <- resp[keep_row, , drop = FALSE]

  if (nrow(resp_cal) < 20) {
    res$status <- 400
    return(list(error = "Too few respondents after removing perfect/zero/incomplete scores (need at least 20)."))
  }

  # ---- Fit model on the calibration sample ----
  mod <- fit_2pl(resp_cal)
  if (inherits(mod, "tam_error")) {
    res$status <- 500
    return(list(error = paste("TAM 2PL model failed:", mod$error)))
  }

  # ---- Remaining diagnostics run on the calibration sample ----
  item_flags <- build_item_flags(resp_cal, mod)
  q3_pairs   <- compute_q3(mod, threshold = 0.2)
  wle        <- wle_summary(mod)
  icc_data   <- build_icc(mod, colnames(resp_cal))

  eap_rel    <- round(as.numeric(mod$EAP.rel), 3)
  wle_rel    <- if (!is.na(wle$reliability)) round(wle$reliability, 3) else NA

  teacher_msg <- build_teacher_message(wle$mean, wle$sd, wle$reliability)

  # ---- Response (flat, consistent snake_case) ----
  list(
    items        = item_flags,
    correlations = q3_pairs,
    model = list(
      type             = "2PL",
      package          = "TAM",
      eap_reliability  = eap_rel,
      eap_text         = text_eap(eap_rel),
      wle_reliability  = wle_rel,
      wle_text         = text_wle_rel(wle_rel),
      wle_mean         = round(wle$mean, 3),
      wle_mean_text    = text_mean(wle$mean),
      wle_sd           = round(wle$sd, 3),
      wle_sd_text      = text_sd(wle$sd)
    ),
    diagnostics          = diag_info,
    icc                  = icc_data,
    message_for_teachers = teacher_msg,
    metadata             = body$metadata
  )
}


#* Health check
#* @get /
function() {
  list(status = "ok", service = "DLTPT IRT", version = "1.1")
}
