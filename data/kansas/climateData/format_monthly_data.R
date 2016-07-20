rm(list=ls(all=TRUE))

#set file dir in control code *or*
T<-read.csv("c:/repos/driversdata/data/kansas/climateData/monthly_temp.csv")
P<-read.csv("c:/repos/driversdata/data/kansas/climateData/monthly_ppt.csv")

names(T)<-c("year",paste(names(T)[2:13],seq(1,12,1),sep="_"))
library(reshape2)
t<-reshape(T, varying=c(names(T)[2:13]), direction="long", sep="_", v.names="meanTemp", new.row.names=NULL)
names(t)[2]<-"Month"
t<-t[,-4]

names(P)<-c("year",paste(names(T)[2:13],seq(1,12,1),sep="_"))
p<-reshape(P, varying=c(names(P)[2:13]), direction="long", sep="_", v.names="PPT", new.row.names=NULL)
names(p)[2]<-"Month"
p<-p[,-4]

dat<-merge(p,t)
dat<-dat[with(dat, order(year, Month)), ]
dat<-na.omit(dat)

head(dat)

names(dat)<-c("year","month","Ppt","Tmid")

library(lattice)
xyplot(Tmid~month, groups=year, data=dat)
xyplot(Ppt~month, groups=year, data=dat)

#export interpolated file
write.csv(dat, "c:/repos/driversdata/data/kansas/climateData/monthClim.csv")
