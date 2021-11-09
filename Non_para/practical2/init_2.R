#Comparison of density estimation for different kernels and bandwidth
library(ks)
library(ggplot2)
theme_set(theme_light())

#Generating data from a mixture normal
set.seed(17)
N<-1000
#setting the means and the standard deviations
mus<-c(0, 1.5, 4)
sds<-sqrt(c(1, 0.4, 2))
#Generating samples
samples<-rnorm.mixt(n=N, mus = mus, sigmas = sds, props = c(0.35, 0.25, 0.4))
summary(samples)

true.val<-function(x)
{
  val<-0.35*dnorm(x,mus[1],sds[1])+0.25*dnorm(x, mus[2], sds[2])+0.4*dnorm(x, mus[3], sds[3])
  return(val)
}
x=seq(-5,10, .1)
truth<-true.val(x)
png(filename = "hist_samples.png", width = 1200, height = 600)
hist(samples, freq = F, ylim =c(0,0.25))
#checking true density graph
lines(x, truth, col ="red", lwd =2)
legend("topright", col = "red", lwd =2, legend = c("True density"))
dev.off()

#Using kernel estimators

#First using the standard thumbrule Silverman's bandwidth with all kernels
ker_rect<-density(samples, bw = "nrd0", kernel = "rectangular")
ker_tri<-density(samples, bw = "nrd0", kernel = "triangular")
ker_biw<-density(samples, bw = "nrd0", kernel = "biweight")
ker_gauss<-density(samples, bw = "nrd0", kernel = "gaussian")
ker_cos<-density(samples, bw = "nrd0", kernel = "cosine")
ker_epn<-density(samples, bw = "nrd0", kernel = "epanechnikov")

ker.init<-list(ker_rect, ker_tri, ker_biw, ker_gauss, ker_cos, ker_epn)

x.new<-rep(ker_rect$x, times = 7)
truth_1<-true.val(ker_rect$x)
y<-cbind(ker_rect$y, ker_tri$y, ker_biw$y, ker_gauss$y, ker_cos$y, ker_epn$y, truth_1)
y<-as.vector(y)
ker<-rep(c("Rectangular","Triangular", "Biweight", "Gaussian", "cosine", "Epanechnikov", "True density"), each = 512)
ker_dat<-data.frame(x.new,y,ker)
  
ker_plot<-ggplot(data = ker_dat, aes(x=x.new, y=y, group = ker, colour = ker))+geom_line()+scale_colour_manual(values = c("sky blue", "blue", "purple", "darkred", "red", "orange", "black"))+
guides()+ggtitle("Density plots based on different kernels")+xlab("x")+ylab("Estimated f(x)")

png(filename = "densityplot_1.png", width = 1000, height = 600)  
plot(ker_plot)
dev.off()

#Get MISE and loocv for different bandwidth


