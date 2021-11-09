#Setting the folder containing all the files are set as working directory
setwd("~/Documents/Non para/MSE compare of estimate of locations")

#run the Source.R script to load the required functions
source("Source.R", echo=T)

#disabling exponential notation
options(scipen = 999)

#parameter values to be specified
theta=13
seed=23
m=100 #number of sample sets
n=100 #number of observation in each sample set
  
for (k in 1:5)
{
  temp=gen_func(m,n,k-1,theta,seed)
  for (l in 1:6)
  {
    MSE[k,l]=mean((temp[,l]-theta)^2)
    V[k,l]=var(temp[,l])
  }
}
detail=c(m,n,theta,seed)
names(detail)=c("m","n","Theta","Seed")
MSE=round(MSE, 4)
V=round(V, 4)
output=list(detail,MSE,V)
names(output)=c("Detail","MSE","Variance")

ARE[,1]=output$Variance[,2]/output$Variance[,1]
ARE[,2]=output$Variance[,2]/output$Variance[,6]
ARE[,3]=output$Variance[,1]/output$Variance[,6]

output[["ARE"]]=round(ARE, 4)

output
output.100.100=output #storing outputs for a specific m, n.

