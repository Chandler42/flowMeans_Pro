MergeLabels <- function (Labels, ListOfLabels, i, j, TI, TJ) 
{
      # make sure i < j
      if (i > j) {
            temp <- i
            i <- j
            j <- temp
      }
      if (TI > TJ) {
            temp <- TI
            TI <- TJ
            TJ <- temp
      }
      
      
      Max <- max(Labels)
      ITJ = which(ListOfLabels == j)
      IMax = which(ListOfLabels == Max)
      ListOfLabels[IMax] <- ListOfLabels[TJ]
      ListOfLabels[ITJ] <- ListOfLabels[TI]
      Labels[which(Labels == j)] <- i
      Labels[which(Labels == Max)] <- j
      return(list(Labels = Labels, ListOfLabels = ListOfLabels))
}