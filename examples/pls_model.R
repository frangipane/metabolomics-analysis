# example_data.csv

## load PLS model scripts in script_library
base.dir = "R"
source(file.path(base.dir, "plsModel.R"), chdir=T)
source(file.path(base.dir, "shuffleModel.R"), chdir=T)
source(file.path(base.dir, "permutationPlots.R"), chdir=T)


#==================================================================================
# preliminary data processing

file ="example_data.csv" 

# BLQ and blanks are read in as NA
rawdata = read.table(file, header=T, sep=",", skip=1, 
                     na.strings=c("NA","BLQ",""," "))

# rename time column
names(rawdata)[3] = "time"

#==================================================================================
# COMBINE TIMEPOINTS SO THAT ALL TIMEPOINTS FOR ALL SIGNALS ARE INCLUDED FOR EACH
# SAMPLE (FOR EACH COMBINATION OF SUBJECT AND TREATMENT)

tumor.treatment = rawdata[(rawdata$Sample.Name=="tumor") & (rawdata$condition=="treatment"),]
tumor.vehicle = rawdata[(rawdata$Sample.Name=="tumor") & (rawdata$condition=="vehicle"),]


# split data frame by times and rename columns for each set of time points
splitdata = function(x) {
  # remove Sample.Name and condition columns
  x["Sample.Name"] = NULL
  x["condition"] = NULL
  
  # split data frame by times into a list of data frames
  split.data = split(x, x["time"])
  
  for (i in seq_along(split.data)) {
    data_t = split.data[[i]]
    t = names(split.data)[i]
    
    # remove time column
    data_t$time = NULL
    
    # append time point to column names
    names(data_t) = paste0(names(data_t),"_",t)
    
    # create "key" column common to all sets of time data
    data_t$key = rep(1, nrow(data_t))
    
    split.data[[i]] = data_t
  }
  return(split.data)
}

tumor.treatment.split = splitdata(tumor.treatment)
tumor.vehicle.split = splitdata(tumor.vehicle)


library(gtools)
# function performs inner join on common key column to get all combinations of subjects
# input is a list of dataframes split by times, with common key column
comboTimecourse = function(x) {
  for (i in seq(length(x)-1)) {
    if (i==1) joined.data = x[[i]]
    joined.data = merge(joined.data, x[[i+1]], all=T)  
  }
  # remove key column
  joined.data$key = NULL
  
  # sort columns into contiguous blocks
  joined.data = joined.data[,mixedorder(names(joined.data))]
  return(joined.data)
}

tumor.treatment.combo = comboTimecourse(tumor.treatment.split)
tumor.vehicle.combo = comboTimecourse(tumor.vehicle.split)

rownames(tumor.treatment.combo) = paste0('treatment','.',seq(nrow(tumor.treatment.combo)))
rownames(tumor.vehicle.combo) = paste0('vehicle','.',seq(nrow(tumor.vehicle.combo)))

tumor.treatment.combo$response = 1
tumor.vehicle.combo$response = 0

# combine combo data 
all.combo = rbind(tumor.treatment.combo, tumor.vehicle.combo)

# remove columns containing NA's:
#all.combo = all.combo[-which(apply(all.combo, 2, anyNA)==T)]

#all.combo.scaled = all.combo
#all.combo.scaled[1:(ncol(all.combo)-1)] = data.frame(scale(all.combo[1:(ncol(all.combo)-1)]))
#all.combo.scaled$condition = with(all.combo.scaled, as.factor(condition))

all.combo$response = with(all.combo, as.factor(response))

#==================================================================================
# PLS ON COMBINED DATA


model = plsModel(df=all.combo, discrete=TRUE)
realmodel.vars = getPLSModelValues(model, discrete=T)

# confusion matrix
#confusionMatrix(data = model)

## plot VIP
# variable importance - based on weighted sums of the absolute regression 
# coefficients. The weights are a function of the reduction of the sums of 
# squares across the number of PLS components and are computed separately 
# for each outcome. Therefore, the contribution of the coefficients are 
# weighted proportionally to the reduction in the sums of squares.
plot(varImp(model), top=20)


## permutation test for significance of model predictions
shufflemodel.vars = shuffleModel(df = all.combo[ , 1:(ncol(all.combo)-1)], n_shuffle = 2, 
                                 all.combo$response, discrete = TRUE)

## plots of real model predictions compared to predictions on shuffled variables
perm.plots = permutationPlots(shufflemodel.vars, realmodel.vars, discrete = TRUE, n_sd = 1)

print(perm.plots$metric.plot)
print(perm.plots$VIP.plot)
print(perm.plots$coeff.plot)