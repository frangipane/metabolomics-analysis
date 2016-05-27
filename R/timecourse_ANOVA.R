## function timecourse_ANOVA computes two-way ANOVA per metabolite.

## INPUT:
##  df: 
##    dataframe containing (at least) columns "condition", "time", followed by 
##    columns of metabolites
##  met1.idx: 
##    the column index corresponding to the first metabolite in the dataframe
##  writeToFile:
##    Boolean - does user want to write the ANOVA results to file?
##  fout:
##    optional name for anova.df written out to csv file (fout includes .csv extension)

## OUTPUT:
##  anova.df:
##    dataframe containing results of two-way ANOVA
##  anova.csv:
##    anova.df written out to csv file

## create dataframe to store results of two-way ANOVA
## per metabolite (per sample, i.e. do ANOVA separately
## per sample type)

## load function to calculate two-way ANOVA
source("twowayANOVA.R")


timecourse_ANOVA = function(df, met1.idx, writeToFile = TRUE, fout="timecourse_ANOVA.csv") {

  anova.df = data.frame(metabolite = character(),
                        cond.pVal = numeric(),
                        time.pVal = numeric(),
                        interaction = numeric())
    
  #==================================================================================
  ## perform two-way anova for timecourses for a particular sample type (e.g. tumor)
  ## using factors:
  ## - condition (e.g. treatment vs vehicle)
  ## - time (e.g. 1,4,7,24 hours)
  ## Allows for unbalanced data (e.g. 4 replicates at t=24 hrs, and 4 replicates
  ## at (treatment,t=4 hrs), while there are 3 replicates for all other levels)
  
  ## loop through metabolite timecourses for two-way ANOVA
  metabolitenames = names(df)[met1.idx:length(names(df))]
  for (i in seq_along(metabolitenames)) {
    row = twowayANOVA(metabolitenames[i], df)
    if (row!=0) anova.df = rbind(anova.df, row)
  }
  
  
  #==================================================================================
  ## write out results of two-way anova to csv file
  if(writeToFile) write.csv(anova.df, file=fout, row.names=F)
  
  return(anova.df)
}