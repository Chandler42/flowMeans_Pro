library(feature)
library(flowMeans)
library(flowCore)
library(flowPeaks)


# source all functions
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/countModes.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/distanceMatrix.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/inSameCluster.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/MergeLabels.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/nextMerge.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/updateMergedClusters.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/flowMeans.R')
source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro_1.0/MergeMatrix.R')


# debugSource('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/flowMeans.R', encoding = 'UTF-8')
# source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/updateMergedClusters.R')
# source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/MergeLabels.R')
# source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/countModes.R')
# source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/inSameCluster.R')
# source('~/SOTON/Summer Project/R_workspace/flowMeans_Pro/distanceMatrix.R')

data(x)
res <- flowMeansPro(x, varNames=c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)

plot(x[,c(3,4)], col=res@Labels[[res@MinIndex+1]], pch=20)

# The Mins vector contains the minimum distances between the merged clus-
# ters at each iteration:
plot(res@Mins, xlab='Iteration', ylab='Distance')


# The changepointDetection function can be used to find the change point in
# the chart of minimum distances and iterations to select the correct number of
# clusters. The MinIndex value indicates the index of the change point:
plot(res@Mins, xlab=' ', ylab=' ', xlim=c(1, res@MaxN),
     ylim=c(0, max(res@Mins)))

head(res@Mins)

ft<-changepointDetection(res@Mins)
# Add Straight Lines to a Plot
abline(ft$l1)
abline(ft$l2)
# Set or Query Graphical Parameters
par(new = TRUE)
plot(ft$MinIndex+1, res@Mins[ft$MinIndex+1], col='red', xlab='Iteration', ylab='Distance')


# The Label vector indicates the cluster membership labels. The plot function
# can be used to visualize this:
plot(x, res, c("FL1.H", "FL2.H"))
plot(x, res, c("FL1.H", "FL2.H","FL3.H", "FL4.H"),pch='.')



