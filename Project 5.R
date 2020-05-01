#Project 5
rm(list=ls())
setwd("~/ECON 524")
library(tidyverse)
library(numDeriv)
library(lmtest)
library(leaps)
load("C:/Users/kgard/OneDrive/Documents/ECON 524/games2010.Rdata")

#This function takes as input a data frame in the same format as the "games" dataframe that I provided for Project 5.
#It creates and returns a new data frame in a format similar to project 1 / project 3.
#The code is taken directly from the project 3 solution.
#The new data frame that this function returns has one row for every team with columns for wins, losses, and opponents (and some other stuff).
#The vector of opponents has names instead of numbers (so it is similar to my project 3 solution not project 1).
gendata = function(q){
  awaygames = summarise(group_by(q,awayteam),num=n(),numwins=sum(!homewin))
  homegames = summarise(group_by(q,hometeam),num=n(),numwins=sum(homewin))
  totalgames = full_join(awaygames,homegames,by=c(awayteam="hometeam"))
  totalgames$num.x[is.na(totalgames$num.x)]=0
  totalgames$num.y[is.na(totalgames$num.y)]=0
  totalgames$numwins.x[is.na(totalgames$numwins.x)]=0
  totalgames$numwins.y[is.na(totalgames$numwins.y)]=0
  totalgames$wins = totalgames$numwins.x+totalgames$numwins.y
  totalgames$losses=totalgames$num.x+totalgames$num.y-totalgames$numwins.x-totalgames$numwins.y
  
  
  homespread = summarise(group_by(q,hometeam),o1=first(awayteam),o2=nth(awayteam,2),o3=nth(awayteam,3),o4=nth(awayteam,4),o5=nth(awayteam,5),o6=nth(awayteam,6),o7=nth(awayteam,7),o8=nth(awayteam,8),o9=nth(awayteam,9)) 
  awayspread = summarise(group_by(q,awayteam),o1=first(hometeam),o2=nth(hometeam,2),o3=nth(hometeam,3),o4=nth(hometeam,4),o5=nth(hometeam,5),o6=nth(hometeam,6),o7=nth(hometeam,7),o8=nth(hometeam,8),o9=nth(hometeam,9)) 
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

#This function takes as input a data frame in the format outputted by the gendata function above.
#It computes and returns the Colley rankings based on the input data frame.
#It is taken directly from the solution to project 3.
#This function will work as-is for Model 1 in Project 5.
#However, it would need to be modified for Model 2 to include the extra parameter gamma.
#G=Gamma
#G=.5 For Model 1
colley_matrix = function(totalgames,G){
  C <- matrix(integer(length(totalgames$awayteam)^2),nrow=length(totalgames$awayteam))
  rownames(C)=totalgames$awayteam
  colnames(C)=totalgames$awayteam
  b <- numeric(length(totalgames$awayteam))
  for(i in 1:length(totalgames$awayteam)){
    C[i,i]=2+totalgames$wins[i]+totalgames$losses[i]
    for(j in totalgames$opponents[[i]]){
      C[i,j]=C[i,j]-2*(1-G)
    }
    b[i]=1+G*totalgames$wins[i]-(1-G)*totalgames$losses[i]
  }
  r <- solve(C,b)
  return(r)
}

#games<-games[train,]
totalgames<-gendata(games)
rank<-colley_matrix((gendata(games)),.5)

games$winteam=0
for(i in 1:length(games$awayteam)){
  if(games$awayscore[i]>games$homescore[i]){
    games$winteam[i]=games$awayteam[i]
  } else{
    games$winteam[i]=games$hometeam[i]
  }
}


loglikefun=function(theta){
  G=theta[2]
  Lam=theta[1]
  LL<-vector()
  rank<-colley_matrix((gendata(games)),G)
  for( i in 1:nrow(games)){
    if(games$homewin[i]=="TRUE"){
      Hi<-1
    }else{
      Hi<-0
    }
    
    Hrank<-rank[games$hometeam[i]]
    Arank<-rank[games$awayteam[i]]
    LL[i]<-Hi*Lam*Hrank + (1-Hi)*Lam*Arank - log(exp(Lam*Arank)+exp(Lam*Hrank)) 
  }
  ll1=sum(-LL)
  return(ll1)
  
}

sv=c(.5,.5)
loglikefun(sv)
#G restricted to 0.5 in Model 1
result1<-nlminb(sv,loglikefun,lower = c(0,.5),upper = c(Inf,.5))
result2<-nlminb(sv,loglikefun,lower = c(0,0),upper = c(Inf,1))