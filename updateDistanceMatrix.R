# distanceMatrix: the distances between each clusters
# MaxCovN: Maximum number of points, used for calculating the covariance.
updateDistanceMatrix <- function (x, mat, covMatrixList, Label, Mahalanobis, MaxCovN, updateI, updateJ, updateMax, means, quantity) 
{
      n <- max(Label)
      oldMat <- mat
      mat = matrix(0, n, n)
      covMatrixList[[updateJ]] <- covMatrixList[[updateMax]]
      # update the covariance matrix of cluster updateI
      
      covMatrixList[[updateI]] <- (covMatrixList[[updateI]]*quantity[updateI] + covMatrixList[[updateJ]]*quantity[updateJ] + 
                                         (means[updateI,]-means[updateJ,])%*%t(means[updateI,]-means[updateJ,])* quantity[updateI]*quantity[updateJ]/(quantity[updateI]+quantity[updateJ])) / (quantity[updateI]+quantity[updateJ])
      
      covMatrixList[[updateJ]] <- covMatrixList[[updateMax]]
      # update means and quantity
      means[updateI,] <- colMeans(x[which(Label == updateI), ])
      means[updateJ, ] <- means[updateMax, ]
      quantity[updateI] <- (quantity[updateI]+quantity[updateJ])
      quantity[updateJ] <- quantity[updateMax]
      
      for (i in 1:n) {
            for (j in 1:n) {
                 
                  
                         if (Mahalanobis) {
                               Li <- x[which(Label == i), ] # get points belong to cluster i
                               # Lj <- x[which(Label == j), ]
                               
                               
                               
                                     mat[i, j] = mean(mahalanobis(Li, means[j,], covMatrixList[[i]]))  
                                     
                               }
                                     
                              # mahalanobis distance from centre of Lj to Li (? why use covariance of Li, should be Lj)
                        
                        else mat[i, j] = sqrt(sum(colMeans(Li) - colMeans(Lj))^2) # euclidean distance
                  }
      }
            
      
      return(list(mat = mat, covMatrixList = covMatrixList, means = means, quantity = quantity))
}