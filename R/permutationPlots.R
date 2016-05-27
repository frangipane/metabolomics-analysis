## permutationPlots.R
## Evaluate significance of PLS model by plotting results against fits to permuted responses, e.g. 
## permuted treatment classification or IC50's.
##
## INPUT 
##  (2 lists of format returned by getPLSModelValues.R, one for permutations, one for final model):
##  -permute.list: a list consisting of results from permutations (against with the final model should
##    be evaluated) with the following values:
##      *metric.permute: a vector of ROC or RMSE results from fitting PLS model to permutations 
##        of response labels
##      *VIP.permute: matrix of VIP from PLS model per permutation (variable are columns, permutations are rows)
##      *coeff.permute: matrix of regression coefficients from PLS model per permutation 
##        (variable are columns, permutations are rows)
##  -final.list: a list consisting of results from final model with the following values
##      *metric.final: numeric, single metric (ROC or RMSE) from fitting model to predict responses
##      *VIP.final: numeric vector (length equal to number of variables), VIP per variable from final model
##      *coeff.final: numeric vector (length equal to number of variables), regression coefficients
##        per variable from final model
##  -discrete: boolean, are responses discrete?
##  -n_sd: numeric (>0), cutoff for number of standard deviations a final model VIP or coefficient has to 
##      be from the mean in order to be considered significant and plotted
##
## RETURNS:
## A list of three plots: 
##    -metric.plot: histogram of metric (ROC/RMSE)
##    -VIP.plot: bargraph of variable importance projection
##    -coeff.plot: bargraph of regression coefficients


# base.dir = "script_library"
# source(file.path(base.dir,"permutation_metric.R"))
# source(file.path(base.dir,"permutation_VIP.R"))
# source(file.path(base.dir,"permutation_regressionCoeff.R"))
source("permutation_metric.R")
source("permutation_VIP.R")
source("permutation_regressionCoeff.R")

permutationPlots = function(permute.list, final.list, discrete, n_sd=1) {

  metric.permute = permute.list$metric
  VIP.permute = permute.list$VIP
  coeff.permute = permute.list$coeff
  
  metric.final = final.list$metric
  VIP.final = final.list$VIP
  coeff.final = final.list$coeff
  
  metric.plot = permutation_metric(metric.final, metric.permute, discrete)
  #print(metric.plot)
  
  VIP.plot = permutation_VIP(VIP.final, VIP.permute, n_sd=n_sd)
  #print(VIP.plot)
  
  coeff.plot = permutation_regressionCoeff(coeff.final, coeff.permute, n_sd=n_sd)
  #print(coeff.plot)
  
  return(list(metric.plot=metric.plot, VIP.plot=VIP.plot, coeff.plot=coeff.plot))
}