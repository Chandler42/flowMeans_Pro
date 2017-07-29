library(flowMeans)
library(flowPeaks)

data(concave)
plot(concave, col=concave.cid)

# set the number of lines of margin 
par(mar=c(5,5,4,3)) 

#flowMeans
ptm <- proc.time()
fp <- flowMeans(concave, c("x", "y"))
time_flowMeans <- proc.time()-ptm
time_flowMeans
scoreMeans <- evalCluster(concave.cid, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeans
plot(concave, col=fp@Labels[[fp@MinIndex+1]])

# use other parameters
# fp <- flowMeans(concave, c("x", "y"), MaxN = 10)
# plot(concave, col=fp@Labels[[1]])
# fp@MinIndex
# plot(concave, col=fp@Labels[[5]])



# flowPeaks
ptm <- proc.time()
fp <- flowPeaks(concave[,1:2])
time_flowPeaks <- proc.time()-ptm
time_flowPeaks
scorePeaks <- evalCluster(concave.cid, fp$peaks.cluster, method = "Fmeasure")
scorePeaks
plot(concave, col=fp$peaks.cluster)

#flowMeansPro
ptm <- proc.time()
fp <- flowMeansPro(concave, c("x", "y"))
time_flowMeansPro <- proc.time()-ptm
time_flowMeansPro
scoreMeansPro <- evalCluster(concave.cid, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeansPro
plot(concave, col=fp@Labels[[fp@MinIndex+1]])