# example_data.csv

base.dir = "R"
source(file.path(base.dir, "ADtest.R"))
source(file.path(base.dir, "plotQQ.R"))
source(file.path(base.dir, "timecourse_ANOVA.R"), chdir=T)
source(file.path(base.dir, "twowayANOVA.R"))
source(file.path(base.dir, "groupMeans_timecourse.R"))

out.dir = "inspect_stats_output"
dir.create(out.dir)

#==================================================================================
# preliminary data processing

file ="example_data.csv" 

# BLQ and blanks are read in as NA
rawdata = read.table(file, header=T, sep=",", skip=1, 
                     na.strings=c("NA","BLQ",""," "))

# rename time column
names(rawdata)[3] = "time"

#==================================================================================
## Q-Q PLOT CHECKS FOR NORMALITY

## approximately equivalent to plotting:
##  n = length(vec)
##  ( x = qnorm(seq(n)/(1+n)) , y = sort(vec))

plotQQ(rawdata, metabolite.name = "M1", "tumor", "vehicle")
plotQQ(rawdata,  metabolite.name = "M1", "tumor", "vehicle", logtransf=T)

#==================================================================================
## Anderson-Darling tests FOR NORMALITY

## Critical values for given unknown mean and variance:
## for alpha= 5%, reject H0 if A^2 > 0.752 (D'Agostino/wiki) for sample size n>=8
## for alpha= 10%, reject H0 if A^2 > 0.631 (D'Agostino/wiki) for sample size n>=8

ADresults = ADtest(rawdata, metabolite.name = "M1", "tumor", "vehicle")
ADresults

#==================================================================================
## perform two-way anova for timecourses in each sample type using factors:
## - condition (treatment vs vehicle)
## - time (1,4,7,24 hours)
## We have unbalanced data (e.g. 4 replicates at t=24 hrs, and 4 replicates
## at (treatment,t=4 hrs), while there are 3 replicates for all other levels),
## which is accounted for by using type II sum of squares in twowayANOVA(),
## but assuming no interaction b/w condition and time.

## df for input into timecourse_ANOVA() must contain "time" and "condition" columns
anova.tumor = timecourse_ANOVA(rawdata[rawdata$Sample.Name=="tumor", ], 
                               met1.idx = 4, writeToFile = T, fout = file.path(out.dir,"tumor_ANOVA.csv"))
anova.plasma = timecourse_ANOVA(rawdata[rawdata$Sample.Name=="plasma", ], 
                               met1.idx = 4, writeToFile = T, fout = file.path(out.dir,"plasma_ANOVA.csv"))

#==================================================================================
## calculate means and standard error of means per sample name, per vehicle, per time

groupStats = groupMeans_timecourse(rawdata)
mean.data = groupStats$means
sem.data = groupStats$sem

#==================================================================================
## plot timecourses of metabolites which show significant differences between 
## treatment and/or time factors according to two-way ANOVA

## get the union of metabolite names that occur in anova.plasma and anova.tumor
# metabolites = union(anova.tumor$metabolite, anova.plasma$metabolite)
# 
# ## create list of time course plots per metabolite
# plots.list = vector(mode="list", length(metabolites))
# 
# ## iterate over metabolites selected by ANOVA
# ## print to pdf
# pdf("exampledata_timecourse_differencesANOVA.pdf", width=7.5)
# for (i in seq_along(plots.list)) {
#   idx = which(names(mean.data) == metabolites[i])
#   
#   plots.list[[i]] = plotMetabolite_meanTimecourse(mean.data, sem.data, idx)
#   print(plots.list[[i]])
# }
# dev.off()

#==================================================================================
## plot boxplots of metabolites which show significant differences between 
## treatment and/or time factors according to two-way ANOVA

## get union of metabolites in anova.plasma and anova.tumor 
## with significant cond.pVal
metabolites_cond = union(anova.tumor$metabolite[!is.na(anova.tumor$cond.pVal)],
                         anova.plasma$metabolite[!is.na(anova.plasma$cond.pVal)])
## get union of metabolites in anova.plasma and anova.tumor 
## with significant time.pVal
metabolites_time = union(anova.tumor$metabolite[!is.na(anova.tumor$time.pVal)],
                         anova.plasma$metabolite[!is.na(anova.plasma$time.pVal)])

library("lattice")
## plot boxplots for metabolites conditioned on treatment condition
if (length(metabolites_cond) > 0) {
  boxplots.condition = vector(mode="list", length(metabolites_cond))
  for (i in seq_along(metabolites_cond)) {
    formula=as.formula(paste0(metabolites_cond[i],"~condition|Sample.Name"))
    boxplots.condition[[i]] = bwplot(formula, data=rawdata)
  }
  ## print boxplots to screen
  pdf(file.path(out.dir,"exampledata_boxplots_condition.pdf"), width=7.5)
  lapply(boxplots.condition, print)
  dev.off()
}

## plot boxplots for metabolites conditioned on time
if (length(metabolites_time) > 0) {
  boxplots.time = vector(mode="list", length(metabolites_time))
  for (i in seq_along(metabolites_time)) {
    formula=as.formula(paste0(metabolites_time[i],"~as.factor(time)|Sample.Name"))
    boxplots.time[[i]] = bwplot(formula, data=rawdata)
  }
  ## print boxplots to screen
  pdf(file.path(out.dir,"example_boxplots_time.pdf"), width=7.5)
  lapply(boxplots.time, print)
  dev.off()
}


#==================================================================================
## Tukey-Kramer post-hoc test where ANOVA is significant to see which means are
## significantly different from each other (only time factor since treatment
## factor only has 2 means).  -Kramer=> unequal sample sizes

## use Dunnett's modified version (no assumption of equal population variances)
library("DTK")

# pairwise comparisons of mean times for M1 in tumor, alpha=0.05
DTK_M1 = DTK.test(x=rawdata[rawdata$Sample.Name=="tumor" & rawdata$condition=="vehicle", "M1"],
                         f=rawdata[rawdata$Sample.Name=="tumor" & rawdata$condition=="vehicle", "time"])
DTK.plot(DTK_M1)
