# install.packages("evd") 
library(evd)


dgumbel(-1:2, -1, 0.5)
pgumbel(-1:2, -1, 0.5)
??rGumbel
# Parameter estimation for a distribution with known shape parameters
X <- rGumbel(n = 500, location = 1.5, scale = 0.5)
est.par <- eGumbel(X, method="moments"); est.par
plot(est.par)

# Extracting location and scale parameters
est.par[attributes(est.par)$par.type=="location"]
est.par[attributes(est.par)$par.type=="scale"]

#  Fitted density curve and histogram
den.x <- seq(min(X),max(X),length=100)
den.y <- dGumbel(den.x, location = est.par$location, scale= est.par$scale)
hist(X, breaks=10, probability=TRUE, ylim = c(0,1.1*max(den.y)))
lines(den.x, den.y, col="blue")
lines(density(X))

# Parameter Estimation for a distribution with unknown shape parameters
# Example from; Bury(1999) pp.283-284, parameter estimates as given by Bury are location = 33.5
# and scale = 2.241
data <- c(32.7, 30.4, 31.8, 33.2, 33.8, 35.3, 34.6, 33, 32, 35.7, 35.5, 36.8, 40.8, 38.7, 36.7)
est.par <- eGumbel(X=data, method="numerical.MLE"); est.par
plot(est.par)

# log-likelihood
lGumbel(data, param = est.par)

# Evaluating the precision of the parameter estimates by the Hessian matrix
H <- attributes(est.par)$nll.hessian
var <- solve(H)
se <- sqrt(diag(var)); se