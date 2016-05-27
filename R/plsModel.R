## plsModel.R
## fit a PLS model using the caret package.  ROC is the metric for models predicting discrete 
## responses, RMSE for continouous responses. Leave-one-out cross-validation is used, and data
## is preprocessed to be centered and scaled.
##
## INPUT:
##  -df: a dataframe cast in short format, including the response column labeled, "response"
##  -discrete: boolean, is the response discrete?
##  -response: optional vector of responses (variable to be predicted) of length nrow(df).  Only
##    include if the response column is not already included in df
##
## RETURNS:
## model: an object (PLS model) of class "train" from the caret package.

library("caret")

plsModel = function(df, discrete, response=NULL) {
  ## if response column is included separately, add it to the data frame (with label "response")
  if(length(response)>0) {
    df$response = response
  }
  ## use leave-one-out cross validation.
  ## center and scale variables.
  ## tuneLength is number of latent variables.
  ## due to nested cross-validation, maximum tuneLength is #Observations - 2.
  if(discrete) {
    ctrl = trainControl(classProbs = T,
                       method="LOOCV",
                       summaryFunction = twoClassSummary)
    
    model = train(response ~., 
                  data = df, 
                  method="pls", 
                  preProc=c("center","scale"),
                  tuneLength = nrow(df)-2,
                  trControl = ctrl,
                  metric = "ROC")
  } else {
    ## for continuous response, metric defaults to RMSE
    model = train(response ~.,
                  data = df,
                  method = "pls",
                  preProc = c("center", "scale"),
                  tuneLength = nrow(df)-2,
                  trControl = trainControl(method="LOOCV"))
  }
  return(model)
}