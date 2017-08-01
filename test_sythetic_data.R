library(flowMeans)
library(flowPeaks)
library(scatterplot3d)
library(rgl)

# flowMeansPro use default MaxN
ptm <- proc.time()
fp <- flowMeansPro(syn_data, c("V1", "V2", "V3"), 
                   Standardize = TRUE, addNoise = FALSE)
time_flowMeansPro <- proc.time()-ptm
time_flowMeansPro
#  The MinIndex value indicates the index of the change point
scoreMeansPro <- evalCluster(syn_data$V4, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeansPro
scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, 
              color = fp@Labels[[fp@MinIndex+15]], main = "flowMeansPro")

#flowMeans
ptm <- proc.time()
ptm <- proc.time()
fp <- flowMeans(syn_data, c("V1", "V2", "V3"), 
                   Standardize = TRUE, addNoise = FALSE)
time_flowMeans <- proc.time()-ptm
time_flowMeans
#  The MinIndex value indicates the index of the change point
scoreMeans <- evalCluster(syn_data$V4, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeans
scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, 
              color = fp@Labels[[fp@MinIndex+1]], main = "flowMeansPro")
