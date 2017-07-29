countModes <- function (x, ...) 
{
      # Bandwidth Selectors for Kernel Density Estimation
      # bw.nrd is the more common variation given by Scott (1992), using factor 1.06.
      y <- featureSignif(x, bw = bw.nrd(x), ...)
      count <- 0
      for (i in 2:length(y$curv)) {
            # number of modes is estimated by the number of times that the gradient changes from positive to negative.
            # ?? curv: logical grid for significant curvature
            # I think we should use 
            if (y$curv[i] == 1 && y$curv[i - 1] == 0) 
                  count <- count + 1
      }
      return(list(density = y, NumberOfModes = count))
}