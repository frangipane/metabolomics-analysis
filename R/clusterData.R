## clusterData.R
## cluster rows of a data frame by pearson distances
##
## INPUT: 
##  -df: a dataframe with variables to be clustered on rows
##  -ngroup: integer specifying the number of groups
## OUTPUT: a list with two elements: an object of class hclust ("dendro") and group labels ("groups.pearson")

library("cluster")
library("Hmisc")

clusterData = function(df, ngroup) {
  ## Define clustering function.
  clusterfunc = function(d) hclust(d, method="average")

  ## Define Pearson distances:
  ## 1-Pearson correlation matrix with pairwise deletion, also with significance levels
  dist.pearson = function(x) as.dist(1-rcorr(x, type="pearson")[[1]])

  ## Calculate dendrograms using Pearson's distance
  dendro.pearson = clusterfunc(dist.pearson(t(df)))
  
  ## split data into ngroups
  groups.pearson = cutree(dendro.pearson, k=ngroup)
  
  ## return as list with object of dendrogram class and group labels
  return(list(dendro=dendro.pearson, groups=groups.pearson))
}