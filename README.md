`R/` is a library of functions to aid in metabolomics analysis, including stats, models, and plotting.  The directory `examples/` contains some scripts using the functions defined in `R/`, along with an example of the required data format `example_data.csv` (containing made-up data).

***
The functions in `R/` can be divided into the following categories:

## PLOTS
- plotMetabolite_meanTimecourse.R
- groupMeans_timecourse.R
- plotheatmap2.R
- clusterData.R
- plotDendro.R
- plotLoadings.R
- plotLoadings2d.R
- plotScores.R


## STATS
- ADtest.R
- plotQQ.R
- timecourse_ANOVA.R
- twowayANOVA.R

## MODELS
- plsModel.R
- shuffleModel.R (uses getPLSModelValues.R)
- permutationPlots.R (uses permutation_regressionCoeff.R, permutation_VIP.R, permutation_metric.R)
