set.seed(7031)
require(dlookr)
require(ggplot2)
require(Ecdat)
require(bootstrap)
require(fitdistrplus)
require(GGally)
price <- diamonds$price
n <- length(price)


#-------Summary Statistics-------

#Summary Diagnostics
diagnose(diamonds) #-- No missing values
#Find outliers
diagnose_outlier(diamonds)
plot_normality(diamonds)

#Histogram of price 
ggplot(aes(x=price),data=diamonds)+geom_histogram(binwidth = 100)


#Normality
qqnorm(price) 
qqline(price, col = "red", lwd = 3)
#--The data is non-normally distributed, over dispersed because of outliers and highly skewed to the right
#--Hence, there is a need to use non-parametric models. 
#--Some suitable distributions could be: log normal or weibull. We use log normal disttribution


#Ho : Normal dist, H1 : Does not follow normal dist
# We use one-sample KS test
ks.test(price, pnorm, mean = mean(price), sd = sd(price))

#Try fitting weibull
weibullfit <- fitdist(price, "weibull")
summary(weibullfit)
plot(weibullfit) - #Not a good fit

#Fitting a log  distribution vs normal distribution 
ggplot(diamonds, aes(x=log(price))) + geom_histogram(binwidth=.5)
ggplot(aes(x=price),data=diamonds)+geom_histogram(binwidth = 100)
#Hence we go with log distribution as it a better behaved distribution, much closer to bell curve of a normal distibution



#-----ECDF-----
price.cdf <- ecdf(price)

#calculate the upper and lower bands
alpha <- 0.05
eps <- sqrt((1/(2*n))*log(2/alpha))
grid <- seq(round(min(price)),round(max(price)),length.out = 10000)
upper_level <- price.cdf(grid) + eps
upper_level <- pmin(upper_level,1)
lower_level <- (price.cdf(grid) + eps)
lower_level <- pmax(lower_level,0)

#Draw a 95 percent CF for CDF
plot(price.cdf,las = 1, main = "ECDF for Price", col = "grey", lwd = 4)
lines(grid, lower_level, col="red")
lines(grid, upper_level, col = "darkblue")

##!!!!The band is not very well visible


#--Non Parametric Bootstrap SE & CI
theta_hat <- median(price)

theta <- function(x){
  median(x)
}

theta.NonPboot <- bootstrap(price,3200,theta)
theta.NonPboot_se<- sd(theta.NonPboot$thetastar)
nonPbootstrap_CI<-c(theta_hat-2*theta.NonPboot_se,theta_hat+2*theta.NonPboot_se)

par(mfrow=c(2,2))
#hist(theta.hat_true,probability=T)
hist(theta.NonPboot$thetastar,probability = T)



#MLE
n=length(price)
mu_hat=mean(price)
sigma_hat=sqrt((1/n)*sum((price-mean(price))^2))
MLE=mu_hat+qnorm(0.95)*sigma_hat


#Correlation Plots
#GGally::ggpairs(diamonds)

#Regression
head(diamonds,15)
lm <- lm(I(log(price)) ~ I(carat)  + I(cut) + I(color) + I(clarity), data = diamonds)
summary(lm)



#Hypothesis Test - Two sample -- Test Fx=FY

ggplot(aes(x=price),data=diamonds)+geom_histogram(binwidth = 100)+ ggtitle('Histogram of Diamonds price cut wise')+ylab('Count')+xlab('Diamond Price')+facet_wrap(~cut)+theme_minimal()

#Therefore,
#H0 : No difference in price b/w Very good cut and premium cut
#H1 : Statistically different

require(gtools)
b <- 20000

price.verygood <- diamonds$price[diamonds$cut=="Very Good"] 
price.premium <- diamonds$price[diamonds$cut=="Premium"] 
data_cut <- c(price.verygood,price.premium)
 
sample_length <- length(price.verygood) + length(price.premium)
perm.matrix <- t(replicate(b, sample(data_cut, sample_length))) 
perm.T <- rep(0,b)
for(i in 1:b)#1 indicates rows wise here
{perm.T[i] <-
  abs(mean(perm.matrix[1:length(price.verygood),i])-mean(perm.matrix[1:length(price.premium),i]))
}
p.value<-mean(perm.T>abs(mean(price.verygood)-mean(price.premium)))
p.value


#Posterior 
#-- We assume the prior that the pdf of mean is 1
post_price<-mean(rnorm(5000, mean=mu_hat, sd=sigma_hat/sqrt(n)))

