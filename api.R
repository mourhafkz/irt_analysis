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
#* @serializer json
function(req, res){
  # Parse JSON body
  body <- tryCatch({
    fromJSON(req$postBody)
  }, error = function(e){
    res$status <- 400
    return(list(error = paste("Invalid JSON:", e$message)))
  })
  
  # Validate matrix
  if(is.null(body$matrix$rows) || length(body$matrix$rows) == 0){
    res$status <- 400
    return(list(error = "Empty matrix in payload"))
  }
  
  # Convert JSON matrix to data.frame
  df <- tryCatch({
    # Make sure rows is a list
    rows_list <- if (is.list(body$matrix$rows[[1]])) {
      body$matrix$rows
    } else {
      # if flat vector, wrap it as a single-row list
      list(body$matrix$rows)
    }
    
    mat <- do.call(rbind, rows_list)
    df <- as.data.frame(mat)
    colnames(df) <- body$matrix$columns
    df
  }, error = function(e){
    res$status <- 500
    return(list(error = paste("Cannot convert matrix to data.frame:", e$message)))
  })
  print(df)
  
  # Remove person_id if exists
  resp <- df[, -1]
  
  
  # ----------------------------
  # RUN 2PL MODEL
  # ----------------------------
  mod_2pl <- tryCatch({
    tam.mml.2pl(resp)
  }, error = function(e){
    res$status <- 500
    return(list(error = paste("TAM 2PL model failed:", e$message)))
  })
  
  # ----------------------------
  # DYNAMIC ITEM TABLE
  # ----------------------------
  difficulty <- mod_2pl$xsi$xsi
  discrimination <- mod_2pl$B[, , 1]
  item_results <- data.frame(
    Item = colnames(resp),
    Difficulty = difficulty,
    Discrimination = discrimination,
    PropCorrect = colMeans(resp, na.rm = TRUE)
  )
  
  # Print dynamically in R console
  print(item_results)
  
  
  ability <- tam.wle(mod_2pl)
  
  abilities <- ability$theta
  # 6. Item–total correlations
  total_score <- rowSums(resp)
  item_total_cor <- sapply(resp, function(x)
    cor(x, total_score, use = "pairwise.complete.obs"))
  print(item_total_cor)
  
  
  
  # 7. Item correlation matrix
  item_cor_matrix <- cor(resp)
  # 8. Item fit statistics
  fit <- tam.fit(mod_2pl)
  print(fit$itemfit)
  
  
  # ----------------------------
  # Return JSON
  # ----------------------------
  list(
    items = item_results,
    # persons = df,
    model = list(
      type = "2PL",
      package = "TAM",
      EAP_Reliability = mod_2pl$EAP.rel
    ),
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
