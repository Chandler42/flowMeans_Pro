x <- syn_data[,1:3]
scatterplot3d(x)
for (i in 1:length(x[1, ])) {
      x[, i] <- x[, i] - min(x[, i])
      x[, i] <- x[, i]/max(x[, i])
}

scatterplot3d(x)

# countModes
z <- x[,1]

y <- featureSignif(z)
count <- 0
# the sign of the curvature of a graph is the same as the sign of the second derivative 
for (i in 2:length(y$curv)) {
      # number of modes is estimated by the number of times that the gradient changes from positive to negative.
      # ?? curv: logical grid for significant curvature
      # a positive second derivative is upwardly concave
      # I think we should use gradient instead of curvature
      if (y$curv[i] == 1 && y$curv[i - 1] == 0) 
            count <- count + 1
}
count
plot(y, addKDE = TRUE, addSignifCurvRegion = TRUE)
