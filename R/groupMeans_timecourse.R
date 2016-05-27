## calculate means and standard error of means per sample name, per vehicle, per time

## INPUT:
##  df: dataframe containing columns "Sample.Name", "condition", "time", and metabolites
## OUTPUT:
##  list of two dataframes containing mean.data and sem.data


library(plyr)

groupMeans_timecourse = function(df) {
  
  mean.na.rm = function (x) mean(x, na.rm=T)
  sem = function(x) sqrt(var(x,na.rm=T)/length(x))
  
  # mean per sample type, per condition, per timepoint
  mean.data = ddply(df, 
                    .(Sample.Name, condition, time), 
                    numcolwise(mean.na.rm))
  
  # standard error of the mean per sample type, per condition, per timepoint
  sem.data = ddply(df, 
                   .(Sample.Name, condition, time), 
                   numcolwise(sem))
  
  # group identifier for plotting
  mean.data = data.frame(group=paste0(mean.data$Sample.Name, '.', mean.data$condition), mean.data)
  sem.data = data.frame(group=paste0(sem.data$Sample.Name, '.', sem.data$condition), sem.data)

  return(list(means=mean.data, sem=sem.data))
}