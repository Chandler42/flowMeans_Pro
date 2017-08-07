library(flowMeans)
library(flowPeaks)
library(scatterplot3d)
library(rgl)

scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, 
              color = syn_data$V4, main = "flowMeansPro")

# flowMeansPro use default MaxN
ptm <- proc.time()
fp <- flowMeansPro(syn_data, c("V1", "V2", "V3"), 
                   Standardize = TRUE, addNoise = FALSE, MaxN = NA)
time_flowMeansPro <- proc.time()-ptm
time_flowMeansPro
#  The MinIndex value indicates the index of the change point
scoreMeansPro <- evalCluster(syn_data$V4, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeansPro
scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, 
              color = fp@Labels[[fp@MinIndex+1]], main = "flowMeansPro")

#flowMeans
ptm <- proc.time()
ptm <- proc.time()
fp <- flowMeans(syn_data, c("V1", "V2", "V3"), 
                   Standardize = TRUE, addNoise = FALSE, MaxN = NA)
time_flowMeans <- proc.time()-ptm
time_flowMeans
fp@MaxN
#  The MinIndex value indicates the index of the change point
scoreMeans <- evalCluster(syn_data$V4, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeans
scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, 
              color = fp@Labels[[fp@MinIndex+1]], main = "flowMeans")

# jump method
testdata <- matrix(c(rnorm(2000),3+rnorm(2000)),byrow=T,ncol=4)
plot(testdata)

temp <- jump(testdata,K = 5, y=c(1.5,2,2.5),rand=10,trace=F)
