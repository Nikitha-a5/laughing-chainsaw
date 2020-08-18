rm(list = ls())
load("C://Users//nikit//Downloads//games2010.Rdata")
library(tidyverse)
library(numDeriv)
envi <- new.env()
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
d$totalgames <- (d$wins + d$losses)

for (i in 1:nrow(d)) {
  envi[[d$awayteam[i]]] <- i
}


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
oowpfuc(opp)
RPI <- function(d){
  opp <- d$opponents
  opps <- 0
  opps2 <- 0
  for (i in 1:nrow(d)) {
   opps[i] <- opprank(opp,i)
  opps2[i] <- oowpfuc(opp,i)
  }
  RPIrank <- .25*(d$wins/d$totalgames)+.5*opps+.25*opps2
  return(RPIrank)
}

RPI4 <- function(vec){
  g <- vec[2]
  t<- vec[1]
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






