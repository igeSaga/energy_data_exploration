library(ggplot2)
library(shiny)
library(reshape2)
library(dplyr)
library(xts)
library(reshape2)
library(Matrix.utils)
library(lubridate)
library(tidyquant)
library(tibbletime)


rm(list=ls())
load("output/allDataAggH_13_15.Rdata")

valRange<-seq(from=1,to = nrow(allData),by = 1)
selApp<-c(1:5)

meanApp<-apply(allData,1,function(x){median(x,na.rm=T)})

allData.s<-allData[,selApp]


time<-as.POSIXct(row.names(allData))

time<-data.frame(index=time)%>%
  as_tbl_time(index = index)%>%
  mutate(hour=as.numeric(strftime(index, format="%H")),
         weekDay=lubridate::wday(as.Date(index),week_start=1),
         week = as.numeric(format(as.Date(index),"%W")),
         month = as.numeric(format(as.Date(index),"%m")),
         season = cut(month,breaks = seq(0,12,by = 2),labels = c(1,2,3,3,4,1)),
         year = as.numeric(format(as.Date(index),"%Y")),
         MonthTag=factor(month,levels=as.character(1:12),
                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         ordered=TRUE),
         Wday=lubridate::wday(as.Date(index),week_start=1),
         WdayTag=factor(Wday,levels=rev(1:7),
                        labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                        ordered=TRUE))

levels(time$season)<-c("Winter","Spring","Summer","Autumn")

allData.l<-melt(allData.s[valRange,])
names(allData.l)<-c("time","appart","energy")

allData.l<-rbind(allData.l,data.frame(time=row.names(allData.s),appart=rep(NA,nrow(allData.s)),energy=meanApp))

allData.l<-allData.l%>%
  mutate(time=as.POSIXct(time))%>%
  left_join(time,by=c("time"="index"))

saesonal<-c("StundeTag")

if(saesonal=="Stunde"){
  allData.agg<-allData.l%>%
    group_by(appart,hour)%>%
    summarise(energy=median(energy,na.rm=T))
  
  ggplot(allData.agg)+
    geom_path(aes(x=hour,y=energy, color=appart))+
    labs(x="time of day", y="kWh")+
    theme_minimal()+
    theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
    scale_color_discrete(na.value="black")
}

if(saesonal=="StundeTag"){
  allData.agg<-allData.l%>%
    group_by(appart,hour,WdayTag)%>%
    summarise(energy=median(energy,na.rm=T))
  
  ggplot(allData.agg)+
    geom_path(aes(x=hour,y=energy, color=appart))+
    facet_wrap(.~WdayTag,nrow = 1)+
    labs(x="time of day", y="kWh")+
    theme_minimal()+
    theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
    scale_color_discrete(na.value="black")
}

if(saesonal=="StundeTagSeason"){
  allData.agg<-allData.l%>%
    group_by(appart,hour,WdayTag,season)%>%
    summarise(energy=median(energy,na.rm=T))
  
  ggplot(allData.agg)+
    geom_path(aes(x=hour,y=energy,color=appart))+
    facet_grid(season~WdayTag)+
    labs(x="time of day", y="kWh")+
    theme_minimal()+
    theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
    scale_color_discrete(na.value="black")
}