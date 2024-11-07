#### Jackknife
```{r, message=FALSE, warning=FALSE}
#variance function
variance.fn <- function(X){ return(mean((X - mean(X))^2))}

attach(Auto)
X = Auto$mpg
variance.fn(X) # biased  sample variance
var(X) # R computes an unbiased one

# jackknife
library(bootstrap)
JK <- jackknife(x = X, theta = variance.fn)
JK$jack.bias

# jackknife estimator = bias- original estimate
variance.fn(X) - JK$jack.bias 

# tank example
# Example from tanks
X = c(23, 76, 71, 125, 239)
max.fn = function(X){ return(max(X))}

JK <- jackknife(X, max.fn)
JK$jack.bias
(thetaJK <- max(X) - JK$jack.bias) # jackknife estimate

#### Bootstrap
# Using WW2 tank example data

library(boot)

X <- c(23, 51, 76, 125, 239)
# max function
max.fn <- function(X, subsample) {return(max(X[subsample]))}
# here's also a mean and median function
# mean.fn <-  function( X, sample ){ return( mean( X[sample] ) ) }
# median.fn = function( X, sample ){ return( median( X[sample] ) ) }

(Boot <- boot(X, max.fn, R=1000))

# our original estimator of the max
Boot$t0

# 95% confidence interval for max tank number, it's the interval b/w two quantiles
quantile(Boot$t, c(0.25, 0.975))

#### Another bootstrap example for vaccine efficacy
# Data: X = infection, V = vaccination
n1 = 14134
n0 = 14073
x1 = 11 
x0 = 185
V = c(rep(1,n1),rep(0,n0))
X = rep(0,n1+n0)
X[1:x1] = 1
X[(n1+1):(n1+x0)] = 1

Data = data.frame(X,V)

eff.fn = function(data,Z){
  x = data[Z,1]
  v = data[Z,2]
  p1 = sum( x==1 & v==1 )/sum( v==1 )
  p0 = sum( x==1 & v==0 )/sum( v==0 )
  return( 1-p1/p0 ) 
  }

eff.fn(Data,)

library(boot)
BT = boot( Data, eff.fn, R=100 )
BT # original = efficacy estimate
quantile(BT$t, c(0.025, 0.975)) # confidence interval
