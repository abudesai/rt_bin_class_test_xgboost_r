

## Example for testing
# test_json_string <-
#   '{
#       "gender" : "Male" ,
#       "SeniorCitizen" : 0 ,
#       "Partner" : "Yes" ,
#       "Dependents" : "Yes" ,
#       "tenure" : 70  ,
#       "PhoneService" : "Yes" ,
#       "MultipleLines" : "Yes",
#       "InternetService" : "No" ,
#       "OnlineSecurity" : "No internet service" ,
#       "OnlineBackup" :  "No internet service" ,
#       "DeviceProtection": "No internet service" ,
#       "TechSupport" : "No internet service",
#       "StreamingTV" : "No internet service",
#       "StreamingMovies" : "No internet service",
#       "Contract" : "Two year",
#       "PaperlessBilling" : "No",
#       "PaymentMethod" : "Mailed check",
#       "MonthlyCharges" : 25.4,
#       "TotalCharges" : 1782.05
# }'


## ---- Initialising libraries ----
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(xgboost)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(glue)))

## Load model
trained_model  <- read_rds('/opt/ml_vol/model/artifacts/model.rds')
encode_these   <- trained_model$variables_to_encode
target_class          <- trained_model$target_class
other_class           <- trained_model$other_class
id_column             <- trained_model$id_column
threshold <- 0.50




prediction_scorer <- function(row) {
  ## Function to get data and return probability
  
  ## initialize scores
  score  <- 0
  
  ## Encode categorical features with number of training encoding
  encodings_tmp <-
    trained_model$encodings %>%
    map(function(x) {
      if (is.data.frame(x)) {
        x[, 2, drop = TRUE] %>% set_names(x[, 1, drop = TRUE])
      } else {
        x
      }
    })
  for (catvar in encode_these) {
    row[[catvar]] <-
      encodings_tmp[[catvar]][row[[catvar]] %>% as.character()]
  }
  
  ## Getting probability
  score <-
    predict(trained_model$mdl,
            data.matrix(row %>% select(
              all_of(trained_model$mdl$feature_names)
            )))
  
  ## return prediction
  score
}




#* @post /infer
function(req) {
  ## grab the request body 'req' and put it into the variable 'row'
  row <- jsonlite::fromJSON(req$postBody) %>% as_tibble()
  row %>% glimpse()
  
  ## placeholder for JSON string to be printed at the end
  result <-
    tibble(
      predicted_class_prob = 0,
      predicted_class = '',
      warnings = ''
    )
  
  ## parameters that we need
  necessary_params <- trained_model$mdl$feature_names
  
  ## if we do NOT have all we need...
  if (!all(necessary_params %in% names(row))) {
    result$predicted_class_prob <- 0
    result$predicted_class <- ''
    result$warnings <- 'Some necessary features are missing'
    
  } else {
    ## keep only the necessary parameters
    row <- row[necessary_params]
    
    ## if any of the necessary parameters are null...
    if (row %>% sapply(is.null) %>% any()) {
      result$predicted_class_prob <- 0.55
      result$predicted_class <- 'UNCOVERED'
      result$warnings <-
        paste('The following required parameters were NULL:',
              null_parameters)
      
    } else {
      predicted_class_prob <- prediction_scorer(row)
      print(predicted_class_prob)
      result$predicted_class <-
        if_else(predicted_class_prob >= 0.5 ,
                get("target_class"),
                get("other_class"))
    
    result$predicted_class_prob  <- 
     if_else(predicted_class_prob >= 0.5 ,
                predicted_class_prob %>% round(5),
                1 - predicted_class_prob %>% round(5)) 
    
  }
  
  c(result$predicted_class_prob,
    result$predicted_class,
    result$warnings)
}
}




#* @get /ping
#* @serializer json list(auto_unbox=TRUE)
endpoint.healthz <- function(req) {
  return("it's working perfectly")
}
