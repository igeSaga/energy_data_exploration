library(ggplot2)
library(xts)
library(zoo)
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr)


rm(list=ls())


dataPath<-"/Volumes/data$/ta/60 FuE/6040 ZIG/604050 Projekte/60405010 Laufende/1121201_SCCER_FEEBD-I/06-Modelling_Simulation_Testing/Inputfiles/Suurstoffi/Energy Hub/Messdaten/Datenordner_WWZ/"
codes<-read.csv("data/sensor_codes.csv")
# 
# #list all csv in folder
# temp <- list.files(path=dataPath,pattern="*.csv")
# 
# #read each csv into a list
# myfiles <- lapply(temp, function(x){read.delim(file=paste(dataPath,x,sep=""), sep=";")})
# 
# #ignore every second column
# myfiles <- sapply(myfiles, function(x){
#   x<-x[,-seq(from=3,to=ncol(x),by=2)]
#   return(x)
# })
# 
# #only "ausspeisung"
# myfiles <- sapply(myfiles, function(x){
#   cols<-x[2,2:ncol(x)]=="Aussp"
#   x<-x[,c(TRUE,cols)]
#   return(x)
# })
# 
# #ignore the header lines
# myfiles <- sapply(myfiles, function(x){
#   x<-x[8:nrow(x),]
#   return(x)
#   })
# 
# #clean time-stamp
# myfiles<-sapply(myfiles, function(x){
#   stamp<-gsub(" b","",x$ZP)
#   x$ZP<-stamp
#   return(x)
#   })
# 
# #clean time-stamp II
# myfiles<-sapply(myfiles, function(x){
#   stamp<-strtrim(x$ZP,16)
#   x$ZP<-stamp
#   return(x)
#   })
# 
# #only appartements
# codes<-codes[codes$type=="whg",]
# myfiles<-sapply(myfiles, function(x){
#   wCol<-names(x)%in%codes$sensor
#   wCol[1]<-TRUE
#   x<-x[,wCol]
#   return(x)
# })
# 
# save(myfiles,file="output/inputList.Rdata")
# 
# allNames<-unlist(sapply(myfiles,function(x){rbind(names(x))}))
# allNames<-unique(allNames[-1])
# 
# allTimes<-unlist(sapply(myfiles,function(x){rbind(as.character(x[,1]))}))
# allTimes<-unique(allTimes)
# 
# allData<-matrix(data = rep(NA,length(allNames)*length(allTimes)),ncol = length(allNames),dimnames = list(allTimes,allNames))
# 
# #i<-5
# for(i in 1:length(myfiles)){
#   print(i)
#   myDat<-myfiles[[i]]
#   #j<-10
#   for(j in 2:ncol(myDat)){
#     house<-names(myDat)[j]
#     if(house=="CH1021501234571357980000000000000"){
#       print(paste(min(myDat[,1])))
#       print(paste(max(myDat[,1])))
#     }
#     allData[myDat[,1],house]<-as.numeric(as.character(myDat[,j]))
#   }
# }
# 
# save(allData,file="output/allData.Rdata")
# 
# # #check summer time
# # time<-as.POSIXct("29.03.2014 02:00", format="%d.%m.%Y %H:%M", tz = "GMT")
# # attributes(time)$tzone <- "Europe/Zurich"
# # time<-time - 3600
# # time

load("output/allData.Rdata")
load("output/inputList.Rdata")

ts_heatmap <- function(dat,time,title){
  data.frame(Date=as.Date(time), kWh=dat) %>%
    setNames(c("Date","kWh")) %>%
    dplyr::mutate(
      Year=lubridate::year(Date),
      Month=lubridate::month(Date),
      # I use factors here to get plot ordering in the right order
      # without worrying about locale
      MonthTag=factor(Month,levels=as.character(1:12),
                      labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
      # week start on Monday in my world
      Wday=lubridate::wday(Date,week_start=1),
      # the rev reverse here is just for the plotting order
      WdayTag=factor(Wday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
      Week=as.numeric(format(Date,"%W")),
      Hour=as.POSIXlt(time)$hour
    ) %>%
    # ok here we group by year and month and then calculate the week of the month 
    # we are currently in
    dplyr::group_by(Year,Month,Hour,WdayTag,MonthTag) %>% 
    #dplyr::mutate(Wmonth=1+Week-min(Week)) %>% 
    #dplyr::ungroup() %>% 
    summarise(kWh = sum(kWh,na.rm=T)) %>% 
    ggplot(aes(x=Hour, y=WdayTag, fill = kWh)) + 
    geom_tile(colour = "white") + 
    facet_grid(Year~MonthTag) + 
    scale_fill_gradient(low="blue", high="red") +
    labs(x="time of day", y=NULL)+
    ggtitle(title)+
    theme_minimal()+
    theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))
}

time<-as.POSIXct(as.character(rownames(allData)), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<-time - 3600

ind<-2
house <- codes[codes$sensor==colnames(allData)[ind],]
house <- paste("Haus: ",house$house," / Flaeche: ",house$fl," / Zimmer: ",house$zimmer,sep="")
dat<-allData[,ind]

# lets see
ts_heatmap(dat,time, house)

# 
# 
# 
# 
# # #check for NA
# # allTimes[which(is.na(time))]
# # rownames(allData)[which(is.na(time))]
# # as.POSIXct(allTimes[which(is.na(time))], format="%d.%m.%Y %H:%M")
# # as.POSIXct("30.03.2014 02:00", format="%d.%m.%Y %H:%M")
# 
# ind<-70
# colnames(allData)[ind]
# nas<-is.na(allData[,ind])
# dat<-allData[!nas,ind]
# t<-time[!nas]
# dat.t<-xts(dat,t)
# plot(dat.t)
# 
# ts.zoo<-zoo(allData[,ind],order.by = time, frequency = 24*4*7)
# ts<-ts(ts.zoo)
# tsdisplay(diff(ts))
# 
# mts.dec <-decompose(ts.zoo)
# mts.stl<-stl(ts.zoo, s.window = "periodic")
# monthplot(mts.stl)
# plot(mts.stl)
# plot(mts.dec)
# 
# spec.pgram(ts,type="h")
