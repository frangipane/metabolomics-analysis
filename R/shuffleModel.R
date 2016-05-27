## shuffleModel.R
## shuffle test to assess significance of feature variable importance and regression coefficients, along with
## a goodness of fit metric like ROC/RMSE.
## Use permutationPlots.R to plot results of shuffleModel(...).

## Maximum number of distinct permutations of discrete response variable, given n repetitions of class '1' and 
## m repetitions of class '0':
## (n+m)!/(n!*m!)

## INPUT (for shuffleModel):
##  -df: data frame cast in short format without a response variable column (the number of predictors is
##      equal to ncol(df))
##  -n_shuffle: an integer, the number of response permutations to be generated
##  -response: column to be predicted from df.  a vector with the same number of rows as 
##      df (e.g. treatment to be predicted, or IC50, per row)
##  -discrete: boolean, is the response to be predicted discrete or continuous?
##
## RETURNS:
##  A list consisting of:
##  -metric: numeric vector, containing goodness of fit of PLS model per permutation (for discrete
##      response, metric is ROC; for continuous, metric is RMSE.)
##  -VIP: matrix of VIP from PLS model per permutation (variable are columns, permutations are rows)
##  -coefficients: matrix of regression coefficients from PLS model per permutation 
##      (variable are columns, permutations are rows)

# base.dir = "script_library"
# source(file.path(base.dir,"getPLSModelValues.R"))
# source(file.path(base.dir,"plsModel.R"))
source("getPLSModelValues.R")
source("plsModel.R")

library("permute")

shuffleModel = function(df, n_shuffle=10, response, discrete=T) {
  ## number of predictors/independent variables
  nVars = ncol(df)
  
  ##====================================
  ## allocate space to store results for each shuffle
  
  ## metric of model fitness, e.g. R^2 for continuous response, 
  ## or area under ROC for two-class classification
  metric = vector(mode="numeric", length=n_shuffle)
  
  ## VIP per variable (column), per permutation (row)
  VIP = matrix(nrow=n_shuffle, ncol=nVars)
  colnames(VIP) = colnames(df)
  
  ## regression coefficients (column), per permutation (row)
  coefficients = matrix(nrow=n_shuffle, ncol=nVars)
  colnames(coefficients) = colnames(df)
  
  ##====================================
  ## retrieve set of permuted indices (n_shuffle sets, each of length nrow(df))
  shuffle.idxs = shuffleIndices(nrow(df), n_shuffle)
  
  ## fit model for each permutation
  for (i in seq(n_shuffle)) {
    ## shuffle labels
    df$response = response[shuffle.idxs[i,]]
    
    ## fit model to shuffled outcomes
    model = plsModel(df, discrete)
    
    ## extract model values
    modelvals = getPLSModelValues(model, discrete)
    
    metric[i] = modelvals$metric
    VIP[i,] = modelvals$VIP
    coefficients[i,] = modelvals$coeff
    
    ## display iteration number for every 50 cycles
    if((i %% 50)==0) print(i)
  }
  return(list(metric=metric, VIP=VIP, coefficients=coefficients))
}

## create a set of n_shuffle permutations from row (nObs) indices
shuffleIndices = function(nObs, n_shuffle, seed=2345) {
  set.seed(seed)
  # generate permutations per iteration
  shuffle.idxs = shuffleSet(nObs, n_shuffle)
  
  return(shuffle.idxs)
}