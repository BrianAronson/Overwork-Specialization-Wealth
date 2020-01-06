#Quantile illustrations

x=rnorm(100)
x[1:50]<-ifelse(x[1:50]<1,x[1:50]+1,x[1:50])
x<-abs(x)
x<-ifelse(x>2,x-1,x)
y=x+.5*x^2+rnorm(100)
dat<-data.frame(x=x,y=y)

library(ggplot2)

ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method="lm")

plot(x,y)



x <- seq(0,100,length.out = 100)        # independent variable
sig <- 0.1 + 0.05*x                     # non-constant variance
b_0 <- 6                                # true intercept
b_1 <- 0.1                              # true slope
e <- rnorm(100,mean = 0, sd = sig)      # normal random error with non-constant variance
y <- b_0 + b_1*x + e                    # dependent variable
dat <- data.frame(x,y)
ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method="lm")


ggplot(dat, aes(x,y)) + geom_point() + geom_smooth(method="lm",se = FALSE)
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile(quantiles=.5)
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile(quantiles=.1)
ggplot(dat, aes(x,y)) + geom_point() + geom_quantile(quantiles=.9)

