library(ggplot2)
library(shiny)
library(reshape2)
library(dplyr)
library(xts)
library(reshape2)

rm(list=ls())
load("output/allData.Rdata")

valRange<-seq(from=1,to = nrow(allData),by = 1)
selApp<-c(1:5)


allData.s<-allData[,selApp]
allData.temp<-allData.s
allData.temp[allData.temp==0]<-NA

time2<-as.POSIXct(row.names(allData.s), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time2)$tzone <- "Europe/Zurich"
time2<-time2 - 3600

allData.l<-melt(allData.s[valRange,])
names(allData.l)<-c("time","appart","energy")

time<-as.POSIXct(allData.l$time, format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<-time - 3600
allData.l$time<-time

summaryTime<-data.frame(meanEnergy=apply(allData.temp,1,function(x) mean(x, na.rm=T)))
summaryTime$time<-time2
summaryTime$sdEnergy<-apply(allData.temp,1,function(x) sd(x, na.rm=T))

allData.l<-merge(allData.l,summaryTime,by="time",sort = F)

ggplot() +
  geom_violin(data=allData.l, aes(x = appart, y = energy, color=appart, fill=appart, group=appart))+
  geom_hline(yintercept = median(allData.l$energy), color="grey40", size=1)+
  geom_hline(yintercept = median(allData.l$energy)+sd(allData.l$energy), color="grey40",linetype=2, size=1)+
  theme_minimal()+
  ylab("kWh/15min")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")

# 
# ggplot(allData.l) +
#   geom_area(aes(x = time,y = energy, color=appart, group=appart))+
#   geom_path(aes(x = time,y = meanEnergy, group=1), color="grey40", size=0.5,alpha=0.5)+
#   geom_path(aes(x = time,y = meanEnergy+sdEnergy, group=1), color="grey40", linetype=2, size=0.5,alpha=0.5)+
#   facet_wrap(.~appart,nrow = 1)+
#   theme_minimal()+
#   ylab("kWh/15min")+
#   theme(legend.position = "none",
#         axis.title.x=element_blank())


dat.hm <- allData.l %>%
  dplyr::mutate(
    Year=lubridate::year(time),
    Month=lubridate::month(time),
    MonthTag=factor(Month,levels=as.character(1:12),
                    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
    Wday=lubridate::wday(time,week_start=1),
    WdayTag=factor(Wday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
    Week=as.numeric(format(time,"%W")),
    Hour=as.POSIXlt(time)$hour
  )%>% 
  dplyr::mutate(Tageszeit = cut(Hour, breaks = seq(from=0,to=24,by=4), labels=paste(seq(from=0,to=20,by=4),seq(from=4,to=24,by=4),sep="-") ,
                                ordered_result = T))

dat.hm <-   dat.hm %>%
  group_by(Year,Month,Tageszeit,WdayTag,MonthTag,appart,Week) %>% 
  filter(energy>meanEnergy+sdEnergy)%>%
  summarise(energySum = sum(energy,na.rm=T),countOut=n())

dat.hm <-   dat.hm %>%
  filter(countOut>8) %>%
  group_by(Year,Month,Tageszeit,WdayTag,MonthTag,appart) %>% 
  summarise(energySum=sum(energySum,na.rm=T),countTot=sum(countOut))

dat.hm <-   dat.hm %>%
  mutate(energyMean=energySum/countTot)

p1<-ggplot(dat.hm, aes(x=Tageszeit, y=WdayTag, fill = energyMean)) + 
  geom_tile(colour = "white") + 
  facet_grid(Year+appart~MonthTag) + 
  labs(x="time of day", y=NULL)+
  theme_minimal()+
  theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))

p1

#ggsave(p1,filename = "test.png",width = 20,height = 20,units = "cm",dpi = 300)
