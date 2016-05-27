## plot timecourses for a particular metabolite
## (in general, for all sample types and treatment conditions)

## INPUT:
##    -data.m: a data frame in long format with columns c("time", "concentration", "Sample.Name", "condition")
##    -metabolite: string, the name of the metabolite used for the plot title
## OUTPUT:
##    a ggplot object of timecourses (mean of replicates) for a metabolite, including error bars (standard
##    error of the mean)

library(ggplot2)

plotMetabolite_meanTimecourse = function(data.m, metabolite) {
  timecourse =
    ggplot(data.m, aes(x=time, y=concentration, 
                       group = interaction(Sample.Name, condition),
                       color = interaction(Sample.Name, condition))) +
    stat_summary(fun.y = mean, geom="line") +
    stat_summary(fun.y = mean, 
                 fun.ymin = function(x) mean(x, na.rm=T) - sqrt(var(x, na.rm=T)/length(x)),
                 fun.ymax = function(x) mean(x, na.rm=T) + sqrt(var(x, na.rm=T)/length(x))) +
    xlab("time (hours)") +
    ylab("concentration") +
    ggtitle(metabolite) +
    theme_bw() +
    guides(color = guide_legend(title = "sample/treatment"))
  
  return(timecourse)
}


# plotMetabolite_meanTimecourse = function(mean.data, sem.data, idx) {
# 
#   timecourse =
#     ggplot(data=mean.data, 
#            aes(x=time, y=mean.data[[idx]], group=group, color=group)) +
#     geom_line() +
#     geom_point(size=3) +
#     xlab("time (hours)") +
#     ylab(names(mean.data)[[idx]]) +
#     theme_bw() +
#     geom_errorbar(data=sem.data,
#                   aes(x=time, 
#                       ymin=mean.data[[idx]]-sem.data[[idx]],
#                       ymax=mean.data[[idx]]+sem.data[[idx]]),
#                   width=1,
#                   position=position_dodge(0.2))
#   
#   # print(timecourse)
#   return(timecourse)
# }