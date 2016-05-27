## permutation_regressionCoeff.R
## Plot regression coefficients per variable in the real model against respective mean and standard deviations 
## calculated from permutations.
##
## INPUT:
##  -coeff.permute: matrix of regression coefficients from PLS model per permutation 
##      (variable are columns, permutations are rows)
##  -coeff.final: numeric vector (length equal to number of variables), regression coefficients
##      per variable from final model
##  -removeInsig: boolean, option not to graph variables whose final model coefficients are less than n_sd standard 
##      deviations from the mean coefficient (averaged over permutations)
##  -n_sd: numeric (>0), cutoff for number of standard deviations a final model coefficient has to be from the mean
##      to be considered significant and plotted
##
## RETURNS:
## coefficients.plot: bargraph showing mean regression coefficients (averaged over permutations) 
## as blue dots per variable, standard deviations as error bars, and bars corresponding to regression
## coefficients of final model. (or NA if there are no significant variables)

library("ggplot2")

permutation_regressionCoeff = function(coeff.final, coeff.permute, removeInsig=F, n_sd=1) {

  ## calculate mean and standard deviation of coefficients from permutation results of each variable
  summary.coeffPerVar = data.frame(meanImp = apply(coeff.permute,2, function(x) mean(x, na.rm=T)),
                                   sdImp = apply(coeff.permute,2, function(x) sd(x, na.rm=T)))
  summary.coeffPerVar = 
    within(summary.coeffPerVar, {
      variable = row.names(summary.coeffPerVar)
      realmodel = coeff.final
      })

  ## don't plot variables whose regression coefficients are less than n_sd standard deviations
  ## from their mean
  if(removeInsig) {
    summary.coeffPerVar = 
      subset(summary.coeffPerVar, abs(realmodel - meanImp) > n_sd*sdImp)
  }
  
  ## if there are still some variables left to plot
  if(nrow(summary.coeffPerVar)>0) {
    limits = aes(ymax = meanImp + sdImp, ymin = meanImp - sdImp)
    coefficients.plot =
      ggplot(summary.coeffPerVar, aes(x=variable, y=meanImp)) + 
      geom_bar(aes(y=realmodel), stat="identity", fill="pink") +
      geom_point(color="blue") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle=90, vjust=0, hjust=1), panel.grid.major=element_blank()) + 
      geom_errorbar(limits, color="gray") +
      ylab('regression coefficients')
      return(coefficients.plot)
  } else {
    return(NA)
  }
}