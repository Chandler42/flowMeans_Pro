library(scatterplot3d)
attach(mtcars) # objects in the database can be accessed by simply giving their names.

scatterplot3d(wt,disp,mpg, main = "3D Scatterplot")

# gaussian distribution
library(ggplot2)

lrows <- 100
lcols <- 4
# cluster 1
syn_data1<- array(data = 0, dim = c(lrows, lcols))

# name each column
colnames(syn_data1) <- c("V1","V2","V3","V4")

syn_data1[,1:2] <- matrix( rnorm(lrows*2,mean=0,sd=0.5), lrows, 2)
syn_data1[,3] <- matrix( rnorm(lrows*1,mean=0,sd=0.5), lrows, 1)


#assign values to some columns
syn_data1[,4] <- c(rep(1, lrows))

#cluster 2
syn_data2<- array(data = 0, dim = c(lrows, lcols))

# name each column
colnames(syn_data2) <- c("V1","V2","V3","V4")

syn_data2[,1:2] <- matrix( rnorm(lrows*2,mean=2,sd=0.5), lrows, 2)
syn_data2[,3] <- matrix( rnorm(lrows*1,mean=0,sd=0.5), lrows, 1)


#assign values to some columns
syn_data2[,4] <- c(rep(2, lrows))


#cluster 3
syn_data3<- array(data = 0, dim = c(lrows, lcols))

# name each column
colnames(syn_data3) <- c("V1","V2","V3","V4")

syn_data3[,1:2] <- matrix( rnorm(lrows*2,mean=0,sd=0.5), lrows, 2) 
syn_data3[,3] <- matrix( rnorm(lrows*1,mean=2,sd=0.5), lrows, 1)

#assign values to some columns
syn_data3[,4] <- c(rep(3, lrows))


#cluster 4
syn_data4<- array(data = 0, dim = c(lrows, lcols))

# name each column
colnames(syn_data4) <- c("V1","V2","V3","V4")

syn_data4[,1] <- matrix( rnorm(lrows*2,mean=2,sd=0.5), lrows, 1) 
syn_data4[,2] <- matrix( rnorm(lrows*2,mean=0,sd=0.5), lrows, 1) 
syn_data4[,3] <- matrix( rnorm(lrows*1,mean=0,sd=0.5), lrows, 1)

#assign values to some columns
syn_data4[,4] <- c(rep(4, lrows))

#cluster 5
syn_data5<- array(data = 0, dim = c(lrows, lcols))

# name each column
colnames(syn_data5) <- c("V1","V2","V3","V4")

syn_data5[,1] <- matrix( rnorm(lrows*2,mean=0,sd=0.5), lrows, 1) 
syn_data5[,2] <- matrix( rnorm(lrows*2,mean=2,sd=0.5), lrows, 1) 
syn_data5[,3] <- matrix( rnorm(lrows*1,mean=0,sd=0.5), lrows, 1)

#assign values to some columns
syn_data5[,4] <- c(rep(5, lrows))
syn_data = rbind(syn_data1, syn_data2, syn_data3, syn_data4, syn_data5)

# convert to data frame
syn_data <- as.data.frame(syn_data)

#plot

scatterplot3d(x = syn_data$V1,y=syn_data$V2,z=syn_data$V3, color = syn_data$V4, main = "3D Scatterplot")
# library(rgl)
# plot3d(syn_data$V1, syn_data$V2, syn_data$V3, col=syn_data$V4, size=3)