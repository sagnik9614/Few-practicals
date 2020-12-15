library(psych)
library(nimble)

#function for calculating the max ordered welsh stat
or_mean=function(x)
{
  l=length(x)
  x=order(x,decreasing = F)
  b=rep(0,l)
  for (i in 1:l)
  {
    b[i]=(x[i]+x[l-i+1])/2
  }
  return(max(b))
}



#function for calculating the stats for different distributions
#Choices are described below
#0 for normal
#1 for logistic
#2 for cauchy
#3 for double expo
#4 for unif

gen_func=function(m,n,choice,theta,seed)
{
  set.seed(seed)
  
  #Creating the observation matrix
  X=matrix(NA, nrow = m, ncol = n)
  
  #Creating the matrix of the statist-ics
  stat=matrix(NA, nrow = m, ncol = 6)
  colnames(stat)=c("Median", "Mean", "min plus max mean", "max ordered welsh mean", "Winsorised", "HodgesLehmann")
  
  #taking the choice of the distribution
  if (choice == 0)
  {  
    for (i in 1:m)
    {
      X[i,]=rnorm(n, theta, 1)
    }
  }
  else if (choice == 1)
  {
    for (i in 1:m)
    {
      X[i,]=rlogis(n, theta, 1)
    }
  }
  
  else if (choice == 2)
  {
    for (i in 1:m)
    {
      X[i,]=rcauchy(n, theta, 1)
    }
  }
  
  else if (choice == 3)
  {
    for (i in 1:m)
    {
      X[i,]=rdexp(n, theta, 1)
    }
  }
  
  else if (choice == 4)
  {
    for (i in 1:m)
    {
      X[i,]=runif(n, theta-0.5,theta+0.5)
    }
  }
  
  #Computing the stats
  for (i in 1:m)
  {
    stat[i,1]=median(X[i,])
    stat[i,2]=mean(X[i,])
    stat[i,3]=(min(X[i,])+max(X[i,]))/2
    stat[i,4]=or_mean(X[i,])
    stat[i,5]=winsor.means(X[i,], trim = 0.1)
    stat[i,6]=wilcox.test(X[i,], conf.int = T)$estimate
  }
  
  return(stat)
}

#Creating the Mean-squared error matrix for all the estimators and distributions
MSE=matrix(NA, nrow=5, ncol=6)
colnames(MSE)=c("Median", "Mean", "min plus max mean", "max ordered welsh mean", "Winsorised", "HodgesLehmann")
rownames(MSE)=c("Normal","Logistic","Cauchy","Double Expo","Uniform")

#Creating the variance matrix for all the estimators and distributions
V=matrix(NA, nrow=5, ncol=6)
colnames(V)=c("Median", "Mean", "min plus max mean", "max ordered welsh mean", "Winsorised", "HodgesLehmann")
rownames(V)=c("Normal","Logistic","Cauchy","Double Expo","Uniform")

#Creating the ARE matrix for all distributions b/w Median-mean, HL-mean, HL-median
ARE=matrix(NA, nrow=5, ncol=3)
rownames(ARE)=c("Normal","Logistic","Cauchy","Double Expo","Uniform")
colnames(ARE)=c("Median/Mean", "HL/Mean", "HL/Median")
