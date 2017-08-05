library(feature)
flowMeansPro <- function (x, varNames = NULL, MaxN = NA, NumC = NA, iter.max = 50, 
          nstart = 10, Mahalanobis = TRUE, Standardize = TRUE, Update = "Mahalanobis", 
          OrthagonalResiduals = TRUE, MaxCovN = NA, MaxKernN = NA, 
          addNoise = FALSE) 
{
      
      # transfer input to matrix
      if (is(x, "flowFrame")) {  # Is an Object from a Class?
            if (length(varNames) == 0) {
                  y <- exprs(x)
                  varNames <- colnames(y)
            }
            else {
                  y <- as.matrix(exprs(x)[, varNames])  # turn its argument into a matrix
            }
      }
      else if (is(x, "matrix")) {
            if (length(varNames) == 0) {
                  y <- x
                  if (length(colnames(x)) == 0) 
                        varNames <- "Not Available"
                  else varNames <- colnames(x)
            }
            else {
                  y <- as.matrix(x[, varNames])
            }
      }
      else if (is(x, "data.frame")) {
            if (length(varNames) == 0) {
                  y <- as.matrix(x)
                  varNames <- colnames(x)
            }
            else {
                  y <- as.matrix(x[, varNames])
            }
      }
      else if (is(x, "vector")) {
            y <- matrix(x)
            if (length(varNames) == 0) 
                  varNames <- "Not Available"
      }
      else {
            stop(paste("Object ", as.character(x), " is not of class flowFrame / matrix / data frame!"))
      }
      x <- y
      
      # see if there are elements that are not finite (not infinite and not missing)
      if (length(is.finite(x)) != length(x)) 
            stop("One or more of the values in 'x' are not finite (i.e., are NaN, NA, Inf, or -Inf")
      
      # add noise to input
      if (addNoise) {
            set.seed(546)
            nfactor = 0.05
            x = x + runif(length(x), nfactor * -1, nfactor)  # min, max:	lower and upper limits of the distribution
      }
      
      # standardize to 0 to 1 interval
      # feature scaling, also called unity-based normalization
      if (Standardize) {
            for (i in 1:length(x[1, ])) {
                  x[, i] <- x[, i] - min(x[, i])
                  x[, i] <- x[, i]/max(x[, i])
            }
      }
      
      # the distance function will be updated at each merging iteration with recalculating mahalanobis distances.
      if (Update == "Mahalanobis") {
            if (!Mahalanobis) 
                  Update = "Mean"
      }
      
      # Maximum number of points, used for counting the modes using kernel density estimation. If set to NA 
      # (default), all the points will be used.)
      if (is.na(MaxKernN)) {
            MaxKernN <- length(x[, 1])
      }
      
      # Maximum number of points, used for calculating the covariance. If set to NA (default), all the points will be used.)
      if (is.na(MaxCovN)) {
            MaxCovN <- length(x[, 1])
      }
      
      # Maximum number of clusters. If set to NA (default) the value will be estimated automatically.
      if (is.na(MaxN)) {
            MaxN <- 0
            # # project x onto eigenvectors
            # z <- prcomp(x)
            # PCA <- z$rotation
            # y <- x %*% PCA
            
            # don't project 
            y <- x
           for (i in 1:length(y[1,])){
            MaxN <- (MaxN + countModes(y[1:MaxKernN, i])$NumberOfModes) 
            
            # countModes: number of modes in one dimension. (not eigenvector of the data?
           }

            MaxN <- max(MaxN, 3)
            #concatenate and print
            # cat("MaxN=",MaxN)
      }
      
      # Number of clusters. If set to NA (default) the value will be estimated automatically.
      if (!is.na(NumC)) {
            if (MaxN < NumC) 
                  MaxN = NumC + 10
      }
      
      # The algorithm of Hartigan and Wong (1979) is used by default.
      km <- kmeans(x, centers = MaxN, iter.max = iter.max, nstart = nstart)
      # cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
      Label <- km$cluster
      
      dimensionality <- length(x[1,])
      dM <- distanceMatrix(x, Label, Mahalanobis, MaxCovN)
      mat <- dM$mat
      covMatrixList <- dM$covMatrixList
      Max <- max(mat)
      Mins <- vector() #vector(mode = "logical", length = 0)
      Mats <- list()	# list of mats
      Labels <- list() # list of Labels
      N <- max(Label) # number of clusters
      means <- matrix(0,N,dimensionality) # colmeans of every cluster
      quantity <- vector() # number of points of each cluster
      
      Mats[[1]] <- mat
      Labels[[1]] <- Label
      MergedClusters <- list() # list of number of merged clusters
      ListOfLabels <- c(1:MaxN) # vector of labels
      for(i in 1:N){
            means[i,] <- colMeans(x[which(Label == i),])
            quantity[i] <- length(x[which(Label == i),1])
                  
      }
      
      # merge clusters until all in one
      for (i in 1:MaxN) MergedClusters[[i]] <- c(i) #initialization 
      while (max(Label) > 1) {
            
            if (!is.na(NumC)) 
                  if (max(Label) <= NumC) {
                        Min = min(mat)
                        break
                  }
            # the biggest distance * 2
            Min <- Max * 2
            I <- 0
            J <- 0
            TI <- 0
            TJ <- 0
            if (Update == "None") {# the distance matrix will not be updated
                  temp <- nextMerge(mat, MergedClusters) # cluster indices and distance of nearest clusters
                  Min <- temp$Min
                  TI <- temp$I
                  TJ <- temp$J
                  MergedClusters <- updateMergedClusters(TI, TJ, MergedClusters)
                  I <- ListOfLabels[TI]
                  J <- ListOfLabels[TJ]
            }
            else {
                  # means and number of points of each cluster
                  
                  
                  for (i in 1:N) {
                        for (j in 1:i) {
                              if (i == j) 
                                    next
                              ij <- min(mat[i, j], mat[j, i]) # distance between cluster i and j
                              if (ij < Min) {
                                    Min = ij
                                    I <- i
                                    J <- j
                              }
                        }
                  }
                  TI = I
                  TJ = J
                  
                  Mins[MaxN - N + 1] <- Min # vector of minimum distance 
                  # Label: vector of label of every point; ListOfLabels: all the labels
                  # merged points use small label, and change the name of the max label to the other label's
                  temp <- MergeLabels(Label, ListOfLabels, I, J, TI, TJ)
                  Label <- temp$Label
                  ListOfLabels <- temp$ListOfLabels
                  # N: how many clusters are left.
                  N <- max(Label)
                  updateI <- min(TI,TJ)
                  updateJ <- max(TI,TJ)
                  updateMax <- N+1
                  # update the distance matrix
                  if (Update == "Mahalanobis") 
                        # may do software optimization here 
                        updateDistance <-updateDistanceMatrix(mat,covMatrixList, Label, Mahalanobis, MaxCovN, updateI, updateJ, updateMax,means,quantity)
                        mat = updateDistance$mat
                        covMatrixList <- updateDistance$covMatrixList
                        means <- updateDistance$means
                        quantity <- updateDistance$quantity
                  
                  if (Update == "Mean") 
                        mat = MergeMatrix(mat, I, J)
                  # list to store variable in every iteration
                  Labels[[MaxN - N + 1]] <- Label
                  Mats[[MaxN - N + 1]] <- mat
                  Mins[MaxN - N + 1] <- Min # vector of min distance between clusters
            }
      }
      
      # In R, the lm(), or “linear model,” function can be used to create a simple regression model.
      # Line = lm(formula = YVAR ~ XVAR, data = dataset)
      Line1 = lm(1 ~ 1)
      Line2 = lm(1 ~ 1)
      if (is.na(NumC)) {
            temp <- changepointDetection(Mins, OrthagonalResiduals = TRUE) # spell error "orthognal"
            Line1 <- temp$l1
            Line2 <- temp$l2
            MinIndex <- MaxN - temp$MinIndex
            Label <- Labels[[MaxN - MinIndex + 1]] # label vector of the change point
      }
      if (!is.na(NumC)) {
            MinIndex <- NumC
            Label <- Labels[[MaxN - MinIndex + 1]]
      }
      # new: Generate an Object from a Class
      # where is "Populations"?
      return(new("Populations", Label = Label, Labels = Labels, 
                 MinIndex = MinIndex, MaxN = MaxN, Mats = Mats, Mins = Mins, 
                 Line1 = Line1, Line2 = Line2))
}
