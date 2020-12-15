options(scipen=999)

packages<-c("ggplot2", "tibble")
inst<-packages %in% installed.packages()
if(length(packages[!inst])>0) install.packages(.packages[!inst])

lapply(packages, require, character.only = T)

generating<-function(n, B, mu, sigma, seed)
{
  set.seed(seed)
  
  #generate a sample of size n
  prim.sample<-rnorm(n, mu, sigma)
  
  #Generating jackknife samples from the original
  jk.sample<-matrix(NA, nrow = n, ncol=n-1)
  for (i in 1:n)
  {
    jk.sample[i,]<-prim.sample[-i]
  }
  
  #Generating B bootstrap samples from the original
  boot.sample<-matrix(NA, nrow = B, ncol = n)
  for (i in 1:B)
  {
    boot.sample[i,]<-sample(prim.sample, size = n, replace = T)
  }
  large.sample<-matrix(rnorm(B*n, mu, sigma), nrow = B, ncol = n)
  samples<-list(prim.sample, jk.sample, boot.sample, large.sample)
  return(samples)
}


#estimating function
est<-function(samp, n, B, p)
{
  jk.mat<-matrix(NA, nrow = n, ncol = length(p))
  boot.mat<-matrix(NA, nrow = B, ncol = length(p))
  large.mat<-matrix(NA, nrow = B, ncol = length(p))
  jk.var<-c()
  boot.var<-c()
  large.var<-c()
  
  for (i in 1:n)
  {
    jk.mat[i,]<-quantile(samp[[2]][i,], p)
  }
  for (i in 1:B)
  {
    boot.mat[i,]<-quantile(samp[[3]][i,], p)
    large.mat[i,]<-quantile(samp[[4]][i,], p)
  }
  for ( i in 1:length(p))
  {
    jk.var[i]<-(n-1)*var(jk.mat[,i])
    boot.var[i]<-(n/(n-1))*var(boot.mat[,i])
    large.var[i]<-var(large.mat[,i])
  }
  output<-cbind(p, jk.var, boot.var, large.var)
  colnames(output)<-c("p", "Jackknife", "Bootstrap", "Large_sample")
  output<-as_tibble(output)
  return(output)
}

main<-function(n, B, mu, sigma, seed)
{
  
  if (n<2) stop("n is less than 2")
  
  p.seq<-seq(0, 1, by = 0.01)
  
  samples<-generating(n, B, mu, sigma, seed)
  
  vars<-est(samp=samples, n, B, p=p.seq)
  
  vars<-as_tibble(vars)
  
  p.plot<-ggplot(data=vars, aes(x=p))+
    geom_line(aes(y=Jackknife, colour = "Jackknife"))+
    geom_line(aes(y=Bootstrap, colour = "Bootstrap"))+
    geom_line(aes(y=Large_sample, colour = "Large_sample"))+
    geom_vline(xintercept = p.seq[c(26,51,76)], linetype = 3, colour = "purple")+
    labs(title = "Comparative study of estimated variance of sample pth quantile", x = "p", y = "Estimated variance")
  
  png(filename = "Comparative plot.png", height = 600, width = 800)
  plot(p.plot)
  dev.off()
  
  capture.output(print(samples), file = "Samples.txt")
  capture.output(print(vars, n=length(p.seq)), file = "Output.txt")
  
  return(list(samples, vars))
}
  
