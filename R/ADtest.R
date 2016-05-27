## Anderson-Darling tests FOR NORMALITY
##
## arguments:
##    - df: data frame with columns: Sample.Name, condition, and a column containing observations for
##      one metabolite, i.e. metabolite.name
##    - metabolite.name: a string, the name of the metabolite to be plotted, e.g. "M1"
##    - Sample.type: a string from the "Sample.Name" column df, e.g. "plasma" or "tumor"
##    - condition: a string from the "condition" column of df, e.g. "treatment" or "vehicle"
##    - logtransf: T/F, take the log2 transform of the data?
## values
##    - a list returned by ad.test


library("nortest")

ADtest = function(df, metabolite.name, Sample.type, condition, logtransf = FALSE) {
  data = df[df$Sample.Name==Sample.type & df$condition==condition, metabolite.name]
  if(logtransf) {
    data = log2(data)
    ad.test(data)
  } else {
    ad.test(data)
  }
}