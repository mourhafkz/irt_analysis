#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(jsonlite)
library(TAM)


#* @apiTitle IRT Analysis for DLTPT
#* @apiDescription IRT Analysis for DLTPT

#* Run IRT (2PL) from matrix payload
#* @post /irt
#* @post /irt/
#* @serializer json
function(req, res) {
  
  library(jsonlite)
  library(TAM)
  
  # Parse JSON body
  body <- tryCatch({
    fromJSON(req$postBody)
  }, error = function(e) {
    res$status <- 400
    return(list(error = paste("Invalid JSON:", e$message)))
  })
  
  # Validate matrix
  if (is.null(body$matrix$rows) || length(body$matrix$rows) == 0) {
    res$status <- 400
    return(list(error = "Empty matrix in payload"))
  }
  
  # Convert JSON matrix to data.frame
  df <- tryCatch({
    rows_list <- if (is.list(body$matrix$rows[[1]])) body$matrix$rows else list(body$matrix$rows)
    mat <- do.call(rbind, rows_list)
    df <- as.data.frame(mat)
    colnames(df) <- body$matrix$columns
    df
  }, error = function(e) {
    res$status <- 500
    return(list(error = paste("Cannot convert matrix to data.frame:", e$message)))
  })
  
  # Remove person_id if exists
  resp <- df[, -1, drop = FALSE]
  
  # Run 2PL model
  mod_2pl <- tryCatch({
    tam.mml.2pl(resp)
  }, error = function(e) {
    res$status <- 500
    return(list(error = paste("TAM 2PL model failed:", e$message)))
  })
  
  
  
  # Core item parameters
  discrimination <- mod_2pl$B[, , 1][,2]
  difficulty <- mod_2pl$xsi$xsi
  prop_correct <- colMeans(resp, na.rm = TRUE)
  
  # Item-total correlations
  total_score <- rowSums(resp, na.rm = TRUE)
  item_total_cor <- sapply(resp, function(x) cor(x, total_score, use = "pairwise.complete.obs"))
  
  # Outfit statistics
  fit_stats <- tam.fit(mod_2pl)$itemfit
  outfit <- fit_stats$Outfit
  
  # Assemble flags
  flags <- data.frame(
    Item = colnames(resp),
    Difficulty = difficulty,
    Discrimination = discrimination,
    PropCorrect = prop_correct,
    ItemTotalCor = item_total_cor,
    Outfit = outfit,
    stringsAsFactors = FALSE
  )
  
  flags$Flag <- ""
  flags$Flag[flags$Discrimination < 0.6] <- paste(flags$Flag[flags$Discrimination < 0.6], "LowDisc")
  flags$Flag[flags$PropCorrect > 0.9] <- paste(flags$Flag[flags$PropCorrect > 0.9], "TooEasy")
  flags$Flag[flags$PropCorrect < 0.2] <- paste(flags$Flag[flags$PropCorrect < 0.2], "TooHard")
  flags$Flag[flags$ItemTotalCor < 0.3] <- paste(flags$Flag[flags$ItemTotalCor < 0.3], "LowCorr")
  flags$Flag[flags$Outfit > 1.3] <- paste(flags$Flag[flags$Outfit > 1.3], "Misfit")
  flags$Flag <- trimws(flags$Flag)
  
  # High item correlation check
  item_cor_matrix <- cor(resp, use = "pairwise.complete.obs")
  corr_threshold <- 0.6
  upper_idx <- which(item_cor_matrix > corr_threshold & upper.tri(item_cor_matrix), arr.ind = TRUE)
  
  high_corr_df <- if (nrow(upper_idx) > 0) {
    data.frame(
      Item1 = colnames(item_cor_matrix)[upper_idx[,1]],
      Item2 = colnames(item_cor_matrix)[upper_idx[,2]],
      Correlation = item_cor_matrix[upper_idx],
      Recommendation = "Consider dropping/replacing one for more varied exercise",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(Item1 = character(0), Item2 = character(0), Correlation = numeric(0), Recommendation = character(0))
  }
  
  
  # -----------------------------
  # WLE estimation
  # -----------------------------
  wle_res <- tam.wle(mod_2pl)
  wle_scores <- as.numeric(wle_res$theta)
  wle_se <- as.numeric(wle_res$SE)
  
  # Overall WLE reliability
  theta_var <- var(wle_scores, na.rm = TRUE)
  if (is.na(theta_var) || theta_var == 0) {
    overall_wle_reliability <- NA
  } else {
    overall_wle_reliability <- 1 - mean(wle_se^2, na.rm = TRUE) / theta_var
  }
  
  # Mean and SD of WLE
  mean_wle <- mean(wle_scores, na.rm = TRUE)
  sd_wle <- sd(wle_scores, na.rm = TRUE)
  
  # -----------------------------
  # Rule-based teacher guidance
  # -----------------------------
  # 1. Reliability message
  if (is.na(overall_wle_reliability) || overall_wle_reliability < 0.7) {
    reliability_msg <- "The test is okay for practice or formative assessment, but you cannot confidently rank students based on these results."
    improvement_msg <- paste(
      "To improve reliability, consider:\n",
      "- Adding more discriminating items (discrimination > 0.6)\n",
      "- Avoiding items that are too easy or too hard (prop correct > 0.9 or < 0.2)\n",
      "- Increasing diversity in item content to increase score variance."
    )
  } else if (overall_wle_reliability < 0.8) {
    reliability_msg <- "The test is moderately reliable; use results with some caution."
    improvement_msg <- ""
  } else {
    reliability_msg <- "The test is reliable; scores can be used to identify differences in student ability."
    improvement_msg <- ""
  }
  
  # 2. Ability level
  if (mean_wle > 1) {
    ability_msg <- "The exercise targets high-ability students."
  } else if (mean_wle < -0.5) {
    ability_msg <- "The exercise targets low-ability students."
  } else {
    ability_msg <- "The exercise targets average-ability students."
  }
  
  # 3. Variance check
  if (!is.na(sd_wle) && sd_wle < 0.5) {
    variance_msg <- "Consider adding items that vary more in difficulty to better distinguish students."
  } else {
    variance_msg <- ""
  }
  
  # Combine messages
  teacher_msg <- paste(ability_msg, reliability_msg, variance_msg, improvement_msg, sep = "\n")
  
  # -----------------------------
  # Return JSON
  # -----------------------------
  list(
    items = flags,
    correlations = high_corr_df,
    model = list(
      type = "2PL",
      package = "TAM",
      EAP_Reliability = round(mod_2pl$EAP.rel, 3),
      WLE = list(
        mean_score = round(mean_wle, 2),
        SD_score = round(sd_wle, 2),
        Overall_Reliability = ifelse(is.na(overall_wle_reliability), "NA", round(overall_wle_reliability, 3))
      )
    ),
    message_for_teachers = teacher_msg,
    metadata = body$metadata
  )
}



#* Echo back the input
#* @get /
function(){
  "Hello World!"
}


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}
