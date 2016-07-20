
rm(list=ls(all=TRUE))

#set file dir in control code *or*
setwd("C:/repos/driversdata/data/montana/climateData")
D<-read.csv("daily_climate_data.csv")

data<-data.frame(D$year, D$month, D$day, D$max, D$min, D$precip)
names(data)<-c("year","month","day","Tmax", "Tmin", "Ppt")

data$Tmax<-ifelse(data$Tmax>9999, NA, data$Tmax)
data$Tmin<-ifelse(data$Tmin>9999, NA, data$Tmin)
data$Ppt<-as.numeric(levels(data$Ppt))[data$Ppt] 
data$Ppt<-ifelse(data$Ppt>99, NA, data$Ppt)

D2<-read.csv("NCDC_milescity.csv")
data2<-data.frame(D2$DATE, D2$TMAX, D2$TMIN, D2$PRCP)
names(data2)<-c("Date","Tmax", "Tmin", "Ppt")

data2$year<-as.numeric(substr(data2$Date,start=1,stop=4))
data2$month<-as.numeric(substr(data2$Date,start=5,stop=6))
data2$day<-as.numeric(substr(data2$Date,start=7,stop=8))

data2$Tmax<-ifelse(data2$Tmax>9998|data2$Tmax<(-9998), NA, data2$Tmax)
data2$Tmin<-ifelse(data2$Tmin>9998|data2$Tmin<(-9998), NA, data2$Tmin)
data2$Ppt<-as.numeric(levels(data2$Ppt))[data2$Ppt] 
data2$Ppt<-ifelse(data2$Ppt<(-9998), NA, data2$Ppt)

data2$Ppt<-data2$Ppt/0.039370
data2$Tmax<-(5/9)*(data2$Tmax-32);
data2$Tmin<-(5/9)*(data2$Tmin-32);

subs<-data2[data2$year<1931,]
subs<-data.frame(year=subs$year,month=subs$month,day=subs$day,
                 Tmax=subs$Tmax,Tmin=subs$Tmin, Ppt=subs$Ppt)

dat<-rbind(subs,data)
dat$seqDay<-seq(1,nrow(dat),1)
head(dat)
#replace NA's with means of row before and after
library(zoo)
datan<-dat
datan$Tmax<-na.approx(dat$Tmax)
datan$Tmin<-na.approx(dat$Tmin)
datan$Ppt[is.na(datan$Ppt)]<-0


#See what the function is doing:
#par(mfrow=c(2,1))
#plot(Tmax~seqDay, data=dat[1:1000,], type="l")
#plot(Tmax~seqDay, data=dat[200:400,], type="l")
#plot(Tmax~seqDay, data=dat[290:350,], type="l")
#points(Tmax~seqDay, data=datan[290:350,], type="l",lty=2, col="blue")

#plot(Ppt~seqDay, data=dat[2000:3000,], type="l")
#plot(Ppt~seqDay, data=dat[2650:2780,], type="l")
#points(Ppt~seqDay, data=datan[2650:2780,], type="l",lty=2, col="blue")


#flag data that is interpolated
naData<-is.na(dat)*1
interpYN<-ifelse(rowSums(naData)<1, 0, 1)
datan<-cbind(datan,interpYN)
head(datan)
datan<-datan[,-7]


#Attempt to reduce variable number
#mean of Tmax and Tmin
datan$Tmid<-rowMeans(datan[,4:5], dims = 1)


#export interpolated file
write.csv(datan, "interpClim.csv")
