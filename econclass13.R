load(file = "R Walkthrough/nbasal.RData")
mod_OLS = lm(points~age+exper+expersq+coll,data = data)
summary(mod_OLS)

n = nrow(data)
Y = data$points
X = cbind(rep(1,n,1),data$age,data$exper,data$expersq,data$coll)
k = ncol(X)

loglikefun = function(theta){
  b = theta[1:k]
  s2 = exp(theta[k+1])
  Yhat = X%*%b
  resid = Y - Yhat
  pr = dnorm(resid, mean = ,sd =sqrt(s2))
  ll= sum(log(pr))
  return(-ll)
}
sv = c(X[1,],Y[1])
X%*%sv[1:k]


mod_MLE = optim(sv,loglikefun,method = "BFGS",control = list(maxit=1e6,reltol=1e-10))
mod_MLE
mod_MLE$par
loglikefun(sv)

S = 1000
params = matrix(0,nrow = S,ncol = 6)
for (s in (1:S)) {
I = sample(1:n,n,replace = T)
mod_OLS1 = lm(points~age+exper+expersq+coll,data = data[I,])
params[s,1:5] = mod_OLS1$coefficients
params[s,6] = summary(mod_OLS1)$sigma^2
}

sqrt(diag(vcov(mod_OLS)))
sqrt(diag(cov(params)))
