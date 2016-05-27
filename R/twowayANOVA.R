## function performs a two-way ANOVA on factors condition and time
## INPUT:
##  metabolite: a string
##  df: a dataframe containing metabolite levels, labeled by
##  columns "condition" and "time" (case sensitive)
## RETURNS:
##  a row of a dataframe:
##  row = data.frame(metabolite = metabolite,
##                 cond.pVal = NA,
##                 time.pVal = NA,
##                 interaction = interaction.pval)

#==================================================================================
## Notation:
## SS(B|A) = SS(A,B) - SS(A)
##  "sum of squares for the B main effect after the A main effect and 
##  ignoring interactions"

## Type I Sum of Squares:
## SS(A) for factor A
## Ss(B|A) for factor B
## SS(AB|B,A) for interaction AB

## Type II Sums of Squares:
## SS(A|B) for factor A
## SS(B|A) for factor B
## No significant interaction is assumed.

## load car package to easily obtain type 2 sum of squares
library("car")

twowayANOVA = function(metabolite, df) {
  ## first test for interaction: SS(AB|A,B) = SS(A,B,AB) - SS(A,B)
  ## (using type 1 calculation in base R function)
  formula = as.formula(paste0(metabolite,"~condition*time"))
  model.int = anova(lm(formula, data=df))
  
  ## extract Pvalue for interaction term
  interaction.pval = model.int$"Pr(>F)"[3]
  
  ## if interaction term is not significant, extract type 2 sum of squares
  ## pvalues from additive model (no interactions)
  alpha = 0.05
  if (interaction.pval > alpha) {
    
    ## no interaction model
    formula2 = as.formula(paste0(metabolite,"~condition+time"))
    model.noint = Anova(lm(formula2, data=df),type=2)
    
    ## if a significant difference in groups exists for either treatment or time,
    ## write out Pvalues for the significant factor (condition or time) for that 
    ## particular metabolite
    if (sum(model.noint$"Pr(>F)"[1:2] < alpha) > 0) {
      cond.pVal = model.noint$"Pr(>F)"[1]
      time.pVal = model.noint$"Pr(>F)"[2]
      
      row = data.frame(metabolite = metabolite,
                       cond.pVal = if(cond.pVal < alpha) {cond.pVal} else {NA},
                       time.pVal = if(time.pVal < alpha) {time.pVal} else {NA},
                       interaction = NA)
      
      return(row)
    } else {
      ## no interaction, but non interacting model is not significant
      return(0)
    }
  } else {
    ## there is an interaction
    row = data.frame(metabolite = metabolite,
                     cond.pVal = NA,
                     time.pVal = NA,
                     interaction = interaction.pval)
    return(row)
  }
}