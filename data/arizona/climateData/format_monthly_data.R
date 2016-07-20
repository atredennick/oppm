rm(list=ls(all=TRUE))

#set file dir in control code *or*
T<-read.csv("c:/repos/driversdata/data/arizona/climateData/PRISM_Monthly_Tmean.csv")
P<-read.csv("c:/repos/driversdata/data/arizona/climateData/PRISM_Monthly_ppt.csv")

T<-T[,-c(2,3)]
names(T)[1]<-"quad"

library(reshape2);library(stringr)

t<-reshape(T, varying=c(names(T)[2:ncol(T)]), direction="long", new.row.names=NULL, sep="_")
t<-t[,-4]
elems<-unlist(strsplit(as.character(t$time) , "\\." ) )
m <- as.data.frame(matrix(elems , ncol = 2 , byrow = TRUE ))
names(m)<-c("year","month")
m$month<-as.numeric(m$month)

tdat<-data.frame(quad=t$quad, tmean=t$tmean,m)
tdat$year<-as.numeric(as.character(tdat$year))

P<-P[,-c(2,3)]
names(P)[1]<-"quad"

p<-reshape(P, varying=c(names(P)[2:ncol(P)]), direction="long", new.row.names=NULL, sep="_")
p<-p[,-4]
elems<-unlist(strsplit(as.character(p$time) , "\\." ) )
m <- as.data.frame(matrix( elems , ncol = 2 , byrow = TRUE ))
names(m)<-c("year","month")
m$month<-as.numeric(m$month)

pdat<-data.frame(quad=p$quad, ppt=p$ppt,m)
pdat$month<-as.numeric(pdat$month)
pdat$year<-as.numeric(as.character(pdat$year))

dat<-merge(pdat,tdat)
dat<-dat[with(dat, order(year, month)), ]
dat<-na.omit(dat)

names(dat)<-c("quad","year","month","Ppt","Tmid")

library(lattice)
xyplot(Tmid~month, groups=year, data=dat)
xyplot(Ppt~month, groups=year, data=dat)

#export file
write.csv(dat, "c:/repos/driversdata/data/arizona/climateData/monthClim.csv")
