> rm(list = ls())
> load("C:/Users/nikit/Downloads/apple.RData")
> Y <- matrix(data$ecolbs,ncol = 1)
> X <- matrix(c(rep(1,nrow(data)),data$educ,data$ecoprc,data$regprc,
                +               (data$educ*data$ecoprc)),ncol = 5)
> tX <- t(X)
> b <- (solve(tX%*%X,))%*%(tX%*%Y)
> b
[,1]
[1,]  2.16486407
[2,] -0.01604356
[3,] -3.63938238
[4,]  3.00175061
[5,]  0.05340602
> lm(ecolbs ~ educ + ecoprc + regprc + (ecoprc*educ), data)

Call:
  lm(formula = ecolbs ~ educ + ecoprc + regprc + (ecoprc * educ), 
     data = data)

Coefficients:
  (Intercept)         educ       ecoprc       regprc  educ:ecoprc  
2.16486     -0.01604     -3.63938      3.00175      0.05341  
