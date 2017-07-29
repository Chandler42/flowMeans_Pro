# distanceMatrix: the distances between each clusters
# MaxCovN: Maximum number of points, used for calculating the covariance.
distanceMatrix <- function (x, Label, Mahalanobis, MaxCovN) 
{
      n <- max(Label)
      mat = matrix(0, n, n)
      for (i in 1:n) {
            for (j in 1:n) {
                  Li <- x[which(Label == i), ] # get points belong to cluster i
                  Lj <- x[which(Label == j), ]
                  if (!is.vector(Li)) { # more than one dimension
                        if (Mahalanobis) {
                              MaxCovNumber <- min(MaxCovN, length(Li[, 1])) # Maximum number of points, used for calculating the covariance
                              # mahalanobis distance from centre of Lj to Li (? why use covariance of Li, should be Lj)
                              mat[i, j] = mean(mahalanobis(Li, colMeans(Lj), cov(Li[1:MaxCovNumber, ])))  
                        }
                        else mat[i, j] = sqrt(sum(colMeans(Li) - colMeans(Lj))^2) # euclidean distance
                  }
                  if (is.vector(Li)) { # only one dimension
                        if (Mahalanobis) {
                              MaxCovNumber <- min(MaxCovN, length(Li))
                              if(MaxCovNumber > 1)
                                    # sd standard deviation (missing sqrt? or save time)
                                    mat[i, j] = sqrt(sum(Li - mean(Lj))^2/sd(Li[1:MaxCovNumber]))
                              else 
                                    mat[i, j] = sqrt(sum(Li - mean(Lj))^2)
                        }
                        else mat[i, j] = sqrt((mean(Li) - mean(Lj))^2)
                  }
            }
      }
      return(mat)
}