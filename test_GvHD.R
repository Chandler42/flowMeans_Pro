library(flowMeans)
library(flowPeaks)
library(flowCore)
library(flowViz)
library(scatterplot3d)
library(rgl)
#library(mclust)

#temporally cannot find gold standard of GvHD
data("GvHD")

data(x)
# get the data of sample1
GvHD <- GvHD[[20]]
GvHD <- GvHD@exprs
# [, c("FL1-H", "FL2-H", "FL3-H", "FL4-H", "Time")]

# read fcs data
sample <- read.FCS("001.fcs")


res <- flowMeans(x, c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)
plot(x[,c(3,4)], res, c("FL1.H", "FL2.H"))

# plot3d(barcode$Pacific.blue, barcode$Alexa, barcode$APC, col=barcode$barcode.cid, size=3)
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = barcode$barcode.cid, main = "Barcode data")

# set the number of lines of margin 
# par(mar=c(5,5,4,3)) 

#flowMeans
ptm <- proc.time()
fp <- flowMeans(barcode, c("Pacific.blue", "Alexa", "APC"), addNoise = FALSE)
time_flowMeans <- proc.time()-ptm
time_flowMeans
scoreMeans <- evalCluster(barcode$barcode.cid, fp@Labels[[2]], method = "Fmeasure")
scoreMeans
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = fp@Labels[[2]], main = "flowMeans")

# flowPeaks
ptm <- proc.time()
fp <- flowPeaks(barcode[,1:3])
time_flowPeaks <- proc.time()-ptm
time_flowPeaks
scorePeaks <- evalCluster(barcode.cid, fp$peaks.cluster, method = "Fmeasure")
scorePeaks
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = fp$peaks.cluster, main = "flowPeaks")
