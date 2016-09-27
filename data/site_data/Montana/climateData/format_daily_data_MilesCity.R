rm(list=ls(all=TRUE))

#set file dir in control code *or*
setwd("C:/repos/driversdata/data/montana/climateData")
D<-read.csv("NCDC_milescity.csv")

D2<-read.csv("daily_climate_data_AT.csv")
D2<-na.omit(D2)
D2$precip<-as.numeric(as.character(D2$precip))

data<-data.frame(D$DATE, D$TMAX, D$TMIN, D$PRCP)
names(data)<-c("Date","Tmax", "Tmin", "Ppt")

data$year<-as.numeric(substr(data$Date,start=1,stop=4))
data$month<-as.numeric(substr(data$Date,start=5,stop=6))
data$day<-as.numeric(substr(data$Date,start=7,stop=8))

data$Tmax<-ifelse(data$Tmax>9998|data$Tmax<(-9998), NA, data$Tmax)
data$Tmin<-ifelse(data$Tmin>9998|data$Tmin<(-9998), NA, data$Tmin)
data$Ppt<-as.numeric(levels(data$Ppt))[data$Ppt] 
data$Ppt<-ifelse(data$Ppt<(-9998), NA, data$Ppt)
data$seqDay<-1:nrow(data)

data$Ppt<-data$Ppt/0.039370
data$Tmax<-(5/9)*(data$Tmax-32);
data$Tmin<-(5/9)*(data$Tmin-32);

combo<-merge(data,D2)

par(mfrow=c(1,3))
plot(max~Tmax, data=combo, ylab="Site Tmax", xlab="Miles City Tmax")
abline(a=0,b=1, col="red")

plot(min~Tmin, data=combo, ylab="Site Tmin", xlab="Miles City Tmin")
abline(a=0,b=1, col="red")

plot(precip~Ppt, data=combo, ylab="Site Ppt", xlab="Miles City Ppt")
abline(a=0,b=1, col="red")


#replace NA's with means of row before and after
library(zoo)
datan<-data
datan$Tmax<-na.approx(data$Tmax)
datan$Tmin<-na.approx(data$Tmin)
datan$Ppt[is.na(datan$Ppt)]<-0


#See what the function is doing:
par(mfrow=c(2,1))
plot(Tmax~seqDay, data=data[1:1000,], type="l")
plot(Tmax~seqDay, data=data[200:400,], type="l")
plot(Tmax~seqDay, data=data[290:350,], type="l")
points(Tmax~seqDay, data=datan[290:350,], type="l",lty=2, col="blue")

#plot(Ppt~seqDay, data=data[2000:3000,], type="l")
#plot(Ppt~seqDay, data=data[2650:2780,], type="l")
#points(Ppt~seqDay, data=datan[2650:2780,], type="l",lty=2, col="blue")


#flag data that is interpolated
naData<-is.na(data)*1
interpYN<-ifelse(rowSums(naData)<1, 0, 1)
datan<-cbind(datan,interpYN)
head(datan)
datan<-datan[,-7]


#Attempt to reduce variable number
#mean of Tmax and Tmin
datan$Tmid<-rowMeans(datan[,4:5], dims = 1)


#export interpolated file
write.csv(datan, "interpClim.csv")
