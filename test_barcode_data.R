library(flowMeans)
library(flowPeaks)
library(scatterplot3d)
library(rgl)

data(barcode)
# plot(barcode, col=barcode.cid)
barcode <- cbind(barcode, barcode.cid)
barcode <- subset(barcode, barcode.cid > 0)
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
#  The MinIndex value indicates the index of the change point
scoreMeans <- evalCluster(barcode$barcode.cid, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeans
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = fp@Labels[[fp@MinIndex+1]], main = "flowMeans")

# flowPeaks
ptm <- proc.time()
fp <- flowPeaks(barcode[,1:3])
time_flowPeaks <- proc.time()-ptm
time_flowPeaks
scorePeaks <- evalCluster(barcode$barcode.cid, fp$peaks.cluster, method = "Fmeasure")
scorePeaks
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = fp$peaks.cluster, main = "flowPeaks")

# flowMeansPro
ptm <- proc.time()
fp <- flowMeansPro(barcode, c("Pacific.blue", "Alexa", "APC"))
time_flowMeansPro <- proc.time()-ptm
time_flowMeansPro
#  The MinIndex value indicates the index of the change point
scoreMeansPro <- evalCluster(barcode$barcode.cid, fp@Labels[[fp@MinIndex+1]], method = "Fmeasure")
scoreMeansPro
scatterplot3d(x = barcode$Pacific.blue,y=barcode$Alexa,z=barcode$APC, 
              color = fp@Labels[[fp@MinIndex+1]], main = "flowMeans")