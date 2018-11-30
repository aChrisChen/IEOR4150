##### Code for illustrating the law of large numbers and
##### the central limit theorem
##### written by Professor Dieker of Columbia University




############################################################
# Plot the PMF of (X_1+...+X_n)/n where the X_i are        #
# Bernoulli with success probability p                     #
############################################################

n <- 1
p <- 0.9
a <- seq(0,n)
# factoid: PMF of X_1+...+X_n is binomial(n,p)
pa <- dbinom(a, size=n, prob=p)
barplot(pa,names.arg=a/n, xlab="a", ylab="p(a)", main="PMF of (X_1+...+X_n)/n")


############################################################
# Plot the PDF of (X_1+...+X_n)/n where the X_i are        #
# exponential with rate lambda                             #
############################################################

n <- 1
lambda <- 1.5
x <- seq(0,n*5/lambda,length=1000)
# factoid: PMF of X_1+...+X_n is Gamma with shape parameter n and scale parameter lambda
fx <- dgamma(x, shape=n, rate=lambda)
plot(x/n, fx, type="l", lty=1, xlab="x", ylab="f(x)", main="PDF of (X_1+...+X_n)/n")






############################################################
# Plot the PMF of (X_1+...+X_n)/n where the X_i are        #
# Bernoulli with success probability p                     #
# zoom in on the spike                                     #
############################################################

n <- 10000
p <- 1/pi
a <- seq(0,n)
# factoid: PMF of X_1+...+X_n is binomial(n,p)
pa <- dbinom(a, size=n, prob=p)
intervalAroundSpike <- (a/n>p - 3*sqrt(p*(1-p)/n))&(a/n<p + 3*sqrt(p*(1-p)/n))
asubset <- a[intervalAroundSpike]
pasubset <- pa[intervalAroundSpike]
barplot(pasubset,names.arg=asubset/n, xlab="a", ylab="p(a)", main="PMF of (X_1+...+X_n)/n")


############################################################
# Plot the PMF of (X_1+...+X_n)/n where the X_i are        #
# exponential with rate lambda                             #
# zoom in on the spike                                     #
############################################################

n <- 1000
lambda <- pi
x <- seq(0,n*5/lambda,length=100000)
# factoid: PMF of X_1+...+X_n is Gamma with shape parameter n and scale parameter lambda
fx <- dgamma(x, shape=n, rate=lambda)
intervalAroundSpike <- (x/n>1/lambda - 3*sqrt(1/n)/lambda)&(x/n<1/lambda + 3*sqrt(1/n)/lambda)
xsubset <- x[intervalAroundSpike]
fxsubset <- fx[intervalAroundSpike]
plot(xsubset/n, fxsubset, type="l", lty=1, xlab="x", ylab="f(x)", main="PDF of (X_1+...+X_n)/n")
