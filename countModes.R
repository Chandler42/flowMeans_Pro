countModes <- function (x, ...) 
{
    
      # count local optima 
      y <- density(x, bw = bw.nrd(x))$y
      count <- 0
      for ( i in 2:(length(y)-1) ){
            if ( (y[i] > y[i-1]) & (y[i] > y[i+1]) ) {
                  count <- count + 1
            }
      }
      
      return(list(density = y, NumberOfModes = count))
}