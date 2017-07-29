inSameCluster <- function (I, J, MergedClusters) 
{
      for (i in 1:length(MergedClusters)) if (length(which(MergedClusters[[i]] == 
                                                           I)) + length(which(MergedClusters[[i]] == J)) == 2) # I J are both in the MergedClusters[[i]]
            return(TRUE)
      return(FALSE)
}