## permutation_metric.R
## 
## Plot distribution of permutation results for PLS model metric: ROC/RMSE to compare with metric
## of actual model.

## INPUT:
##  -metric.permute: a vector of ROC or RMSE results from fitting PLS model to permutations of response
##      labels
##  -metric.final: numeric, single metric (ROC or RMSE) from fitting model to predict responses
##  -discrete: boolean, are responses discrete? metric=ifelse(discrete, "ROC", "RMSE")
##
## RETURNS:
## metric.plot: Histogram showing distribution of metrics (ROC/RMSE) from PLS model fitted on permutations


library("ggplot2")

permutation_metric = function(metric.final, metric.permute, discrete) {
  metric.df = data.frame(metric = metric.permute)
  pointmetric = data.frame(finalmodelmetric = metric.final)
  
  metric = ifelse(discrete, "AUROC", "RMSE")
  
  metric.plot = 
    ggplot(metric.df, aes(x=metric)) + 
    geom_histogram() + 
    theme_bw() +
    geom_point(data=pointmetric, aes(x=finalmodelmetric, y=0), color="red", size=5) +
    xlab(metric)
  
  return(metric.plot)
}