# example_data.csv

## load all the scripts in script_library
base.dir = "R"
source(file.path(base.dir, "plotMetabolite_meanTimecourse.R"))
source(file.path(base.dir, "plotheatmap2.R"))
source(file.path(base.dir, "clusterData.R"))
source(file.path(base.dir, "plotDendro.R"))
source(file.path(base.dir, "plotLoadings.R"))
source(file.path(base.dir, "plotLoadings2d.R"))
source(file.path(base.dir, "plotScores.R"))
source(file.path(base.dir, "groupMeans_timecourse.R"))

#==================================================================================
# preliminary data processing

file ="example_data.csv" 

# BLQ and blanks are read in as NA
rawdata = read.table(file, header=T, sep=",", skip=1, 
                     na.strings=c("NA","BLQ",""," "))

# rename time column
names(rawdata)[3] = "time"

#==================================================================================
library("tidyr")

## melt data into tidy, i.e. long, format
tidydata = gather(rawdata, metabolite, concentration, -c(Sample.Name, condition, time))

## get list of metabolites in dataset
metabolites = as.character(unique(tidydata$metabolite))

## index of first metabolite column
#m1.idx = 4


#==================================================================================
# Plot time courses per metabolite and treatment group/condition

## create list of time course plots per metabolite
plots.list = vector(mode="list", length(metabolites))

for (i in seq_along(metabolites)) {
  ## subset data to rows for a particular metabolite
  data.m = tidydata[tidydata$metabolite == metabolites[i], ]
  
  plots.list[[i]] = plotMetabolite_meanTimecourse(data.m, metabolites[i])
  print(plots.list[[i]])
}


#==================================================================================
## calculate means and standard error of means per sample name, per vehicle, per time

stats_per_time = groupMeans_timecourse(rawdata)
mean.data = stats_per_time$means
sem.data = stats_per_time$sem

# mean data centered and scaled to unit variance
mean.data.scaled = mean.data
mean.data.scaled[,5:ncol(mean.data)] = scale(mean.data[,5:ncol(mean.data)])


#==================================================================================
# Reshape data frame of means so that metabolite at each 
# time point becomes a variable/column, do for both raw means and scaled means


# define reshaping function for input data with format like mean.data
library("gtools")

reshapeddata = function(x) {
  x2 = reshape(x, 
          idvar = c("group"),
          timevar = "time",
          direction="wide",
          drop = c("Sample.Name", "condition"),
          sep="_")
  
  rownames(x2) = x2$group
  x2$group = NULL
  
  # reorder columns so metabolite timecourses form contiguous blocks per metabolite
  x2 = x2[,mixedorder(names(x2))]
  
  return(x2)
}

data2 = reshapeddata(mean.data)

data2.scaled = reshapeddata(mean.data.scaled)


#==================================================================================
# CALCULATE PEARSON CORRELATION COEFFICIENTS AND PLOT CORRELATION MATRICES
library(corrplot)

# mean timecourse data (unscaled)
# calculate correlation coefficients between treatments/samples for timecourse matrix
treatment.pearsonr = cor(t(data2), use="pairwise.complete.obs", method="pearson")
treatment.pearsonr.scaled = cor(t(data2.scaled), use="pairwise.complete.obs", method="pearson")

# correlation plot without accounting for p-values
# corrplot(as.matrix(treatment.pearsonr), 
#          type="lower", 
#          method="number")

# since rcorr neads >4 rows, manually compute p-values of correlations 
# between rows of timecourse matrix.
# Example: 4 (rows) choose 2 combinations = 6 pairs
# x=t(data2)
# names(x) = row.names(data2)
# out = as.data.frame(combn(ncol(x), 2, 
#       function(idx) {
#         test = cor.test(x[, idx[1]], x[, idx[2]], method="pearson")
#         return(c(names(x)[idx[1]], names(x)[idx[2]], test$estimate, test$p.value))}))
# row.names(out) = c("col.idx1", "col.idx2", "cor", "pval")
x=t(data2)
PVALS = matrix(data = numeric(), 
           nrow = ncol(x),
           ncol = ncol(x))

idxs = combn(ncol(x),2)
for (i in 1:ncol(idxs)) {
  test = cor.test(x[ ,idxs[1,i]], x[ ,idxs[2,i]], method="pearson")
  PVALS[idxs[1,i], idxs[2,i]] = test$p.value
  PVALS[idxs[2,i], idxs[1,i]] = test$p.value
}

# plot correlations, including p-values
corrplot.mixed(treatment.pearsonr,
         lower = "ellipse",
         upper = "number",
         p.mat = PVALS,
         sig.level = 0.05,
         insig = "blank",
         title = "Correlations between Treatment x Samples")

# - - - - - - - - - - - - - - - - - - - - - - - - - - 
# mean timecourse data (scaled per metabolite)
x2=t(data2.scaled)
PVALS2 = matrix(data = numeric(), 
               nrow = ncol(x2),
               ncol = ncol(x2))

idxs = combn(ncol(x2),2)
for (i in 1:ncol(idxs)) {
  test = cor.test(x2[ ,idxs[1,i]], x2[ ,idxs[2,i]], method="pearson")
  PVALS2[idxs[1,i], idxs[2,i]] = test$p.value
  PVALS2[idxs[2,i], idxs[1,i]] = test$p.value
}

# plot correlations, including p-values
# note, problem with plotting when p.mat is included.  since p-vals are all significant
# anyway in this case, not a problem
corrplot.mixed(treatment.pearsonr.scaled,
               upper = "number",
               lower = "ellipse",
#                p.mat = as.matrix(PVALS2),
#                sig.level = 0.05,
#                insig = "blank",
               title = "Correlations between Treatment x Samples (scaled)")


# - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Anything interesting about correlations between the rows as timepoitns?:
# treatment.pearsonr2 = rcorr(t(as.matrix(mean.data[,5:ncol(mean.data)])),
#                             type="pearson")
# corrplot(as.matrix(treatment.pearsonr2$r),
#          type = "lower",
#          method = "ellipse",
#          p.mat = as.matrix(treatment.pearsonr2$P),
#          sig.level = 0.05,
#          insig = "pch",
#          title = "treatmentxsample per timepoint")

# - - - - - - - - - - - - - - - - - - - - - - - - - - 

# calculate correlation coefficients between metabolites, including p-values

# metabolites.pearsonr = cor(mean.data[,5:ncol(mean.data)], 
#                            use="pairwise.complete.obs", method="pearson")
# plot correlation between metabolites
# corrplot(as.matrix(metabolites.pearsonr), type="lower", method="ellipse")

library(Hmisc)
metabolites.pearsonr2 = rcorr(as.matrix(mean.data[,5:ncol(mean.data)]),
                              type="pearson")
corrplot(as.matrix(metabolites.pearsonr2$r),
         type = "lower",
         method = "ellipse",
         p.mat = as.matrix(metabolites.pearsonr2$P),
         sig.level = 0.05,
         insig = "blank",
         title = "Correlations between Metabolites")


#==================================================================================
# PLOT METABOLITE TIMECOURSES AS HEATMAP WITHOUT CLUSTERING

# First, normalize data by the highest value of each signal/column
mean.data.maxnormalized = mean.data

normalizebymax = function(x) {
  maxpercol = sapply(x, function(y) max(y, na.rm=T))
  normalized = mapply('/',x, maxpercol)
  return(normalized)
}

mean.data.maxnormalized[,5:ncol(mean.data.maxnormalized)] = 
  normalizebymax(mean.data.maxnormalized[,5:ncol(mean.data.maxnormalized)])

# order max-normalized data into contiguous blocks of time courses
data2.norm = reshapeddata(mean.data.maxnormalized)

# heatmap of raw data normalized to highest value per metabolite
heatmap.base = plotheatmap2(as.matrix(data2.norm), 
                            scale = "none",
                            "data normalized to highest value per metabolite")

# heatmap of raw data normalized to highest value per metabolite AND scaled by rows
heatmap.scalebyrow = plotheatmap2(as.matrix(data2.norm),
                                  scale = "row",
                                  "data normalized to highest value per metabolite, \n scaled by row")

#==================================================================================
# Preprocess mean data prior to clustering and PCA

# STORE PROCESSED DATA TYPES IN LIST OF MATRICES

# object of processed data types, to be analyzed with PCA
data.matrices = vector(mode='list', length=2)
# [1] raw data
# [2] raw data, centered and scaled by metabolites mean.data (i.e. mean.data.scaled)
# [3] raw data, scaled by metabolite, by time point
data.matrices[[1]] = data2
data.matrices[[2]] = data2.scaled
#data.matrices[[3]] = scale(data2)

#==================================================================================
# CLUSTER TREATMENT*SAMPLE DATA USING PEARSON DISTANCES

## save results of clustering per data type
## objects.dendro is a list containing "dendro", a list of objects of dendrogram class, and 
## "groups", a list of group labels from clustering.
objects.dendro = vector(mode='list', length=length(data.matrices))

for (i in seq_along(data.matrices)) {
  objects.dendro[[i]] = clusterData(data.matrices[[i]], ngroup = 2)
}

#==================================================================================
# PLOT TREATMENT DENDROGRAMS FROM PEARSON DISTANCES

datatypes = c('raw','scaled by metabolite')

for (i in seq_along(objects.dendro)) {
  print(plotDendro(objects.dendro[[i]]$dendro, objects.dendro[[i]]$groups, datatypes[i]))
}

#==================================================================================
# PLOT HEATMAP WITH CLUSTERING FROM PEARSON DISTANCE

idx=2

heatmap.clustered = 
  plotheatmap2(as.matrix(data.matrices[[idx]]), 
               scale = "row",
               title = paste0(datatypes[idx], ",\n distance = Pearson, and scaled by row"),
               Rowv = as.dendrogram(objects.dendro[[idx]]$dendro),
               dendrogram = "row")

#==================================================================================
# PCA on tumor data
# input matrix for PCA: each sample arranged as a combination of region, treatment, and time.

data.pca = rawdata[,-c(1:3)]
row.names(data.pca) = paste(rawdata$Sample.Name,
                               rawdata$condition,
                               rawdata$time,
                               1:nrow(rawdata),
                               sep=".")

## let's just do pca on tumor data
data.tumor = data.pca[rawdata$Sample.Name=="tumor",]
## should do a check for NA's prior to PCA

## center and scale columns (by metabolite) for PCA
PCA.tumor = prcomp(~., data = data.tumor, center=T, scale=T, na.action=na.omit, tol=NULL)
summary(PCA.tumor)

## Plot scree plot of variances captured per PC
screeplot(PCA.tumor)

## Plot loadings of PC1 and PC2
plotLoadings(PCA.tumor, 1)
plotLoadings(PCA.tumor, 2)

## Plot loadings simultaneously of PC1 and PC2
plotLoadings2d(PCA.tumor, 1, 2)

# Plot scores on PC1 and PC2
plotScores(PCA.tumor, 1, 2, meta=NULL)

