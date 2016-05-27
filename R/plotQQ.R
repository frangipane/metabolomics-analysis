## Q-Q PLOT CHECKS FOR NORMALITY
## approximately equivalent to plotting:
##  n = length(vec)
##  ( x = qnorm(seq(n)/(1+n)) , y = sort(vec))
##
## arguments:
##    - df: data frame with columns: Sample.Name, condition, and a column containing observations for
##      one metabolite, i.e. metabolite.name
##    - metabolite.name: a string, the name of the metabolite to be plotted, e.g. "M1"
##    - Sample.type: a string from the "Sample.Name" column df, e.g. "plasma" or "tumor"
##    - condition: a string from the "condition" column of df, e.g. "treatment" or "vehicle"
##    - logtransf: T/F, take the log2 transform of the data?
## values
##    - a qqplot

plotQQ = function(df, metabolite.name, Sample.type, condition, logtransf = FALSE) {
  data = df[df$Sample.Name==Sample.type & df$condition==condition, metabolite.name]
  if(logtransf) {
    data = log2(data)
    qqnorm(data, main=paste("Normal QQ plot:", 
                            metabolite.name, "-", Sample.type, condition, 
                            "log2 transformed", sep=" "))
    qqline(data)
  } else {
    qqnorm(data, main=paste("Normal QQ plot:", 
                            metabolite.name, "-", Sample.type, condition, 
                            sep=" "))
    qqline(data)
  }
}