rm(list = ls())
load("C://Users//nikit//Downloads//games2010.Rdata")
library(tidyverse)
library(numDeriv)

genncolley = function(q,g){
  awaygames = summarise(group_by(q,awayteam),num=n(),numwins=sum(!homewin))
  homegames = summarise(group_by(q,hometeam),num=n(),numwins=sum(homewin))
  totalgames = full_join(awaygames,homegames,by=c(awayteam="hometeam"))
  totalgames$num.x[is.na(totalgames$num.x)]=0
  totalgames$num.y[is.na(totalgames$num.y)]=0
  totalgames$numwins.x[is.na(totalgames$numwins.x)]=0
  totalgames$numwins.y[is.na(totalgames$numwins.y)]=0
  totalgames$wins = totalgames$numwins.x+totalgames$numwins.y
  totalgames$losses=totalgames$num.x+totalgames$num.y- totalgames$numwins.x-totalgames$numwins.y
  
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
  C <- matrix(integer(length(totalgames$awayteam)^2),nrow=length(totalgames$awayteam))
  rownames(C)=totalgames$awayteam
  colnames(C)=totalgames$awayteam
  b <- numeric(length(totalgames$awayteam))
  for(i in 1:length(totalgames$awayteam)){
    C[i,i]=2+totalgames$wins[i]+totalgames$losses[i]
    
    for (h in 1:(totalgames$wins[i]+totalgames$losses[i])) {
      j = which(opps[[i]][h]==totalgames$awayteam)
      
      C[i,j] <- -2*(1-g)*length(which(paste(opps[[i]],sep="") == totalgames$awayteam[j]))
    }
    b[i] <- 1 + g*totalgames$wins[i] - (1-g)*totalgames$losses[i]
  }
  r <- solve(C,b)
  return(r)
}


llt <- function(pvec){
  b <- pvec[1]
  g <- pvec[2]
  r <- genncolley(games,g)
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

m1 <- nlminb(start = c(10,.5),objective = llt,lower = c(0,0),upper = c(Inf,1))
m2 <- nlminb(start = c(10,.25),objective = llt,lower = c(0,0),upper = c(Inf,1))

sqrt(diag(solve(hessian(llt,m2$par))))
