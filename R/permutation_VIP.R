## permutation_VIP.R
## Plot VIP per variable in the real model against respective mean and standard deviations 
## calculated from permutations.
##
## INPUT:
##  -VIP.permute: matrix of VIP from PLS model per permutation (variable are columns, permutations are rows)
##  -VIP.final: numeric vector (length equal to number of variables), VIP per variable from final model
##  -removeInsig: boolean, option not to graph variables whose final model VIP's are less than n_sd standard 
##      deviations from the mean VIP (averaged over permutations)
##  -n_sd: numeric (>0), cutoff for number of standard deviations a final model VIP has to be from the mean
##      to be considered significant and plotted
##
## RETURNS:
## VIP.plot: bargraph showing mean VIP (averaged over permutations) as blue dots per variable, 
## standard deviations as error bars, and bars corresponding to VIP of final model. (or NA if there
## are no significant variables)


library("ggplot2")

permutation_VIP = function(VIP.final, VIP.permute, removeInsig=T, n_sd=1) {
  # plot importance per variable (all variables)
  summary.VIP = data.frame(meanImp = apply(VIP.permute,2,mean),
                           sdImp = apply(VIP.permute,2,sd))
  summary.VIP = 
    within(summary.VIP, {
      variable = row.names(summary.VIP)
      realmodel = VIP.final
      })
  
  ## don't plot variables whose regression coefficients are less than n_sd standard deviations
  ## from their mean
  if(removeInsig) {
    summary.VIP = 
      subset(summary.VIP, abs(realmodel - meanImp) > n_sd*sdImp)
  }
  
  ## if there are still some variables left to plot
  if(nrow(summary.VIP)>0) {
    limits = aes(ymax = meanImp + sdImp, ymin = meanImp - sdImp)
    VIP.plot = 
      ggplot(summary.VIP, aes(x=variable, y=meanImp)) + 
      geom_bar(aes(y=realmodel), stat="identity", fill="pink") +
      geom_point(color="blue") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle=90, vjust=0, hjust=1), panel.grid.major=element_blank()) + 
      geom_errorbar(limits, color="gray") +
      ylab('variable importance projection')
      return(VIP.plot)
  } else {
    return(NA)
  }
}