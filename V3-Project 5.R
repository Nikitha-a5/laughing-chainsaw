rm(list = ls())
load("C://Users//nikit//Downloads//games2010.Rdata")
library(tidyverse)
library(numDeriv)
gendata = function(q){
  awaygames = summarise(group_by(q,awayteam),num=n(),numwins=sum(!homewin))
  homegames = summarise(group_by(q,hometeam),num=n(),numwins=sum(homewin))
  totalgames = full_join(awaygames,homegames,by=c(awayteam="hometeam"))
  totalgames$num.x[is.na(totalgames$num.x)]=0
  totalgames$num.y[is.na(totalgames$num.y)]=0
  totalgames$numwins.x[is.na(totalgames$numwins.x)]=0
  totalgames$numwins.y[is.na(totalgames$numwins.y)]=0
  totalgames$wins = totalgames$numwins.x+totalgames$numwins.y
  totalgames$losses=totalgames$num.x+totalgames$num.y-
    totalgames$numwins.x-totalgames$numwins.y
  
  
  homespread = summarise(group_by(q,hometeam),o1=first(awayteam),o2=nth(awayteam,2),o3=nth(awayteam,3),
                         o4=nth(awayteam,4),o5=nth(awayteam,5),o6=nth(awayteam,6),o7=nth(awayteam,7),o8=nth(awayteam,8),o9=nth(awayteam,9)) 
  awayspread = summarise(group_by(q,awayteam),o1=first(hometeam),o2=nth(hometeam,2),o3=nth(hometeam,3),
                         o4=nth(hometeam,4),o5=nth(hometeam,5),o6=nth(hometeam,6),o7=nth(hometeam,7),o8=nth(hometeam,8),o9=nth(hometeam,9)) 
  totalspread = full_join(awayspread,homespread,by=c(awayteam="hometeam"))
  
  opps=list()
  home=list()
  for(i in 1:length(totalgames$awayteam)){
    tmp=as.character(totalspread[i,])
    tmp=tmp[-1]
    h=c(F,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,T)
    h=h[!is.na(tmp)]
    tmp=tmp[!is.na(tmp)]
    
    opps[[i]]=tmp
    home[[i]]=h
  }
  colnames(totalgames)=c("awayteam","numaway","numawaywins","numhome","numhomewins","wins","losses")
  
  totalgames$opponents=opps
  totalgames$home=home
  return(totalgames)
}
d <- gendata(games)
opps <- d$opponents
d$totalgames <- (d$wins + d$losses)
envi <- new.env()
for (i in 1:nrow(d)) {
  envi[[d$awayteam[i]]] <- i
}

colley <- function(totalgames,g){
  C <- matrix(integer(length(totalgames$awayteam)^2),nrow=length(totalgames$awayteam))
  rownames(C)=totalgames$awayteam
  colnames(C)=totalgames$awayteam
  b <- numeric(length(totalgames$awayteam))
  for(i in 1:length(totalgames$awayteam)){
    C[i,i]=2+totalgames$wins[i]+totalgames$losses[i]
    
    for (h in 1:(totalgames$wins[i]+totalgames$losses[i])) {
      j = envi[[opps[[i]][h]]]
      
      C[i,j] <- -2*(1-g)*length(which(paste(opps[[i]],sep="") == totalgames$awayteam[j]))
    }
    b[i] <- 1 + g*totalgames$wins[i] - (1-g)*totalgames$losses[i]
  }
  r <- solve(C,b)
  return(r)
}
ll1 <- function(b){
  d <- gendata(games)
  r <- colley(d,.5)
  for (i in 1:nrow(games)) {
    games$awayrank[i] <- r[games$awayteam[i]]
    games$homerank[i] <- r[games$hometeam[i]]
    awayscore <- games$awayscore[i]
    homescore <- games$homescore[i]
    if (awayscore > homescore) {
      games$y[i] <- 0
    } else {
      games$y[i] <- 1
    }
  }
  p <- games$homerank*b
  p2 <- games$awayrank*b
  s <- sum((games$y*p)+((1-games$y)*p2)-log(exp(p)+exp(p2)))
  return(-s)
}
ll2 <- function(pvec){
  b <- pvec[1]
  g <- pvec[2]
  r <- colley(d,g)
  for (i in 1:nrow(games)) {
    games$awayrank[i] <- r[games$awayteam[i]]
    games$homerank[i] <- r[games$hometeam[i]]
    awayscore <- games$awayscore[i]
    homescore <- games$homescore[i]
    if (awayscore > homescore) {
      games$y[i] <- 0
    } else {
      games$y[i] <- 1
    }
  }
  p <- games$homerank*b
  p2 <- games$awayrank*b
  s <- sum((games$y*p)+((1-games$y)*p2)-log(exp(p)+exp(p2)))
  return(-s)
}
m1 <- nlminb(start = 9.25,objective = ll1,lower = 0,upper = Inf)
m2 <- nlminb(start = c(9.5,.425),objective = ll2,lower = c(0,0),upper = c(Inf,1))
m1
nlminb(start = 9.25,objective = ll1,lower = 0,upper = Inf,data = games[1:200])

SE1 <- sqrt(diag((hessian(ll1,m1$par))))
SE2 <- sqrt(diag(solve(hessian(ll2,m2$par))))

opprank <- function(opp,i){
  opw <- rep(0,length(opp[[i]]))
  for (j in 1:length(opp[[i]])) {
    ID = envi[[opp[[i]][j]]]
    t = which(games$awayteam == d$awayteam[i] & games$hometeam == d$awayteam[ID])
    if(length(t) >= 1){
      
      ifelse(games$homewin[t] == T,opw[j] <- (d$wins[ID] - 1)/(d$totalgames[ID]-1),
             opw[j] <- d$wins[ID]/(d$totalgames[ID] - 1))
      
    }else {
      t = which(games$awayteam == d$awayteam[ID] & games$hometeam == d$awayteam[i]) 
      
      ifelse(games$homewin[t] == F,opw[j] <- (d$wins[ID] - 1)/(d$totalgames[ID]-1),
             opw[j] <- d$wins[ID]/(d$totalgames[ID] - 1))
    }
  }
  mean(opw)
}
oowpfuc <- function(opp,i){
  oopw <- 0
  for (j in 1:length(opp[[i]])) {
    ID = envi[[opp[[i]][j]]]
    oopw[j] <- opprank(opp,ID)
  }
  mean(oopw)
}

RPI <- function(d){
  opp <- d$opponents
  opps <- 0
  opps2 <- 0
  for (i in 1:nrow(d)) {
    #opps[i] <- opprank(opp,i)
    opps2[i] <- oowpfuc(opp,i)
  }
  RPIrank <- .25*(d$wins/d$totalgames)+.5*opps+.25*opps2
  return(RPIrank)
}
ll3 <- function(b){
  r <- RPI(d)
  for (i in 1:nrow(games)) {
    games$awayrpi[i] <- r[envi[[games$awayteam[i]]]]
    games$homerpi[i] <- r[envi[[games$hometeam[i]]]]
    awayscore <- games$awayscore[i]
    homescore <- games$homescore[i]
    if (awayscore > homescore) {
      games$y[i] <- 0
    } else {
      games$y[i] <- 1
    }
  }
  p <- games$homerpi*b
  p2 <- games$awayrpi*b
  s <- sum((games$y*p)+((1-games$y)*p2)-log(exp(p)+exp(p2)))
  return(-s)
}

RPI4 <- function(t,g){
  opp <- d$opponents
  owp <- 0
  oowp <- 0
  for (i in 1:nrow(d)) {
    owp[i] <- opprank(opp,i)
    oowp[i] <- oowpfuc(opp,i)
  }
  RPIrank <- t*(d$wins/d$totalgames)+((1-t)*g*owp)+((1-g)*oowp)
  return(RPIrank)
}

ll4 <- function(parms){
  b <- parms[1]
  t <- parms[2]
  g <- parms[3]
  r <- RPI4(t,g)
  for (i in 1:nrow(games)) {
    games$awayrpi[i] <- r[envi[[games$awayteam[i]]]]
    games$homerpi[i] <- r[envi[[games$hometeam[i]]]]
    awayscore <- games$awayscore[i]
    homescore <- games$homescore[i]
    if (awayscore > homescore) {
      games$y[i] <- 0
    } else {
      games$y[i] <- 1
    }
  }
  p <- games$homerpi*b
  p2 <- games$awayrpi*b
  s <- sum((games$y*p)+((1-games$y)*p2)-log(exp(p)+exp(p2)))
  return(-s)
}

m3 <- nlminb(start = 12,objective = ll3,lower = 0,upper = Inf)
m4 <- nlminb(start = c(20,.425,.55),objective = ll4,lower = c(0,0,0),upper = c(Inf,1,1))
nlm(ll4,c(30,.4,.3),hessian = T,)

SE3 <- sqrt(diag(solve(hessian(ll3,m3$par))))
SE4 <- sqrt(diag(solve(hessian(ll4,m4$par))))

aic1 <- 2 * m1$objective + 2 * 1
aic2 <- 2 * m2$objective + 2 * 2
aic3 <- 2 * m3$objective + 2 * 1
aic4 <- 2 * m4$objective + 2 * 3

bic1 <- 2 * m1$objective + 1 * log(nrow(games))
bic2 <- 2 * m2$objective + 2 * log(nrow(games))
bic3 <- 2 * m3$objective + 1 * log(nrow(games))
bic4 <- 2 * m4$objective + 3 * log(nrow(games))

nested21 <- (1 - pchisq(2 * (m2$objective - m1$objective), 1))
nested43 <- (1 - pchisq(2 * (m3$objective - m3$objective), 2))
for (k in 1:10) {
  train = which(kfold != k)
  games <- games[train,]
  nlm(ll1,8,hessian = T)
  
}

