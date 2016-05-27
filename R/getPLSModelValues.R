## getPLSModelValues.R
## extracts model fitness (ROC or RMSE), regression coefficients, and VIP from a PLS model
##
## INPUT: 
##    -model: object of class train, PLS model returned by caret
##    -discrete: boolean, is the outcome variable to be predicted discrete or continuous?
##
## OUTPUT (a list of):
##    -metric: numeric; ROC for discrete responses, RMSE for continuous responses
##    -coeff: vector, regression coefficients per variable
##    -VIP: vector, VIP per variable

getPLSModelValues = function(model, discrete) {
  ## number of components in final model
  num.comp = model$finalModel$ncomp
  
  if (discrete) {
    ## area under ROC using the final number of chosen components
    metric = model$results$ROC[num.comp]
    ## coefficients (corresponding to an outcome of class '1')
    coefficients = model$finalModel$coefficients[,2,num.comp]
  } else {
    ## Rsquared
    metric = model$results$RMSE[num.comp]
    ## coefficients (corresponding to an outcome of class '1')
    coefficients = model$finalModel$coefficients[,,num.comp]
  }
  ## variable importance
  VIP = varImp(model)$importance[[1]]
  
  return(list(metric=metric, coeff=coefficients, VIP=VIP))
}