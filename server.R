library(ggplot2)
library(shiny)
library(reshape2)
library(dplyr)
library(xts)

load("../externalData/allDataAggH_13_15.Rdata")

codes<-read.csv("../externalData/sensor_codes.csv")
codes<-codes[codes$type=="whg",]
codes.l<-paste("whg nr: ",1:nrow(codes)," (haus nr; ",codes$house,", / fl; ",codes$fl," / zim; ",codes$zimmer,")",sep="")

meanApp<-apply(allData,1,function(x){median(x,na.rm=T)})

function(input, output) {
  
  time<-as.POSIXct(rownames(allData))
  
  allData.s<-allData
  allData.s[allData==0]<-NA
  meanTime<-apply(allData.s,1,function(x) mean(x, na.rm=T))
  allData.rel<-allData-meanTime
  
  dat.all <- reactive({
    dat<-allData
    if(input$relAbsId=="rel"){
      dat<-allData.rel
    }
    dat
  })

  dat.prep <- reactive({
    ind <- as.character(input$houseId)
    ind <- strtrim(ind,10)
    ind<-as.numeric(gsub("whg nr: ","",ind))
    dat<-dat.all()
    dat<-dat[,ind]
    dat
  })
  
  dats.prep <- eventReactive(input$go,{
    ind <- as.character(input$houseIds)
    ind <- strtrim(ind,10)
    ind<-as.numeric(gsub("whg nr: ","",ind))
    dat<-dat.all()
    dat<-dat[,ind]
    colnames(dat)<-codes.l[ind]
    dat
  })
  
  app.name <- reactive({
    ind <- as.character(input$houseId)
    ind <- strtrim(ind,10)
    ind<-as.numeric(gsub("whg nr: ","",ind))
    dat<-dat.all()
    appName<-colnames(dat)[ind]
    appName
  })
  
  dat.agg.day <- reactive({
    xts.ts <- xts(dat.prep(),time)
    colnames(xts.ts)<-'kWh'
    
    dat.agg<-as.data.frame(apply.daily(xts.ts,sum))
    dat.agg$time<-as.Date(row.names(dat.agg))
    dat.agg
  })
  
  dat.agg.week <- reactive({
    xts.ts <- xts(dat.prep(),time)
    colnames(xts.ts)<-'kWh'
    
    dat.agg<-as.data.frame(apply.weekly(xts.ts,sum))
    dat.agg$time<-as.Date(row.names(dat.agg))
    dat.agg
  })
  
  dat.agg.month <- reactive({
    xts.ts <- xts(dat.prep(),time)
    colnames(xts.ts)<-'kWh'
    
    dat.agg<-as.data.frame(apply.monthly(xts.ts,sum))
    dat.agg$time<-as.Date(row.names(dat.agg))
    dat.agg
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$selected_var <- renderText({ 
    paste("Appartement Code:",app.name())
  })
  
  output$plot2 <- renderPlot({
    
    ytitle<-"kWh/day"
    dat.agg<-dat.agg.day()
    if(input$aggregId=="w"){
      dat.agg<-dat.agg.week()
      ytitle<-"kWh/week"
    }else if(input$aggregId=="m"){
      dat.agg<-dat.agg.month()
      ytitle<-"kWh/month"
    }
    
    dat.agg$kWh<-dat.agg$kWh/4
    
    ggplot(dat.agg, aes(x = time, y = kWh),group=1) +
      geom_path()+
      geom_vline(aes(xintercept = time),color="black",size=0.02)+
      theme_minimal()+
      ylab(ytitle)
  })
  
  output$plot3 <- renderPlot({
    
    time.s<-time
    dat<-dat.prep()
    time.d<-as.Date(time)
    
    if (!is.null(ranges2$x)) {
      lower <- as.Date(ranges2$x[1])
      upper <- as.Date(ranges2$x[2])
      time.s<-time[time.d>=lower & time.d<=upper]
      dat.s<-dat[time.d>=lower & time.d<=upper]
    }else{
      dat.s<-dat.prep()
    }
    
    dat.s<-data.frame(time=time.s,kWh=dat.s)
    
    ggplot(dat.s, aes(x = time.s, y = kWh)) +
      geom_path()+
      theme_minimal()+
      ylab("kWh")
    })
  
  output$plot4 <- renderPlot({
    time.s<-time
    dat.s<-dat.prep()
    time.d<-as.Date(time)
    
    if (!is.null(ranges2$x)) {
      lower <- as.Date(ranges2$x[1])
      upper <- as.Date(ranges2$x[2])
      
      time.s<-time[time.d>=lower & time.d<=upper]
      dat.s<-dat.s[time.d>=lower & time.d<=upper]
    }
    
    dat.hm<-data.frame(Date=as.Date(time.s), kWh=dat.s) %>%
      setNames(c("Date","kWh")) %>%
      dplyr::mutate(
        Year=lubridate::year(Date),
        Month=lubridate::month(Date),
        MonthTag=factor(Month,levels=as.character(1:12),
                        labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
        Wday=lubridate::wday(Date,week_start=1),
        WdayTag=factor(Wday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
        Week=as.numeric(format(Date,"%W")),
        Hour=as.POSIXlt(time.s)$hour
      )%>%
      group_by(Year,Month,Hour,WdayTag,MonthTag) %>% 
      summarise(kWh = mean(kWh,na.rm=T))

    p1<-ggplot(dat.hm, aes(x=Hour, y=WdayTag, fill = kWh)) + 
      geom_tile(colour = "white") + 
      facet_grid(Year~MonthTag) + 
      labs(x="time of day", y=NULL)+
      theme_minimal()+
      theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))
    
    if(input$relAbsId=="rel"){
      minhm<-min(dat.hm$kWh,na.rm=T)
      maxhm<-max(dat.hm$kWh,na.rm=T)
      p2<-p1+scale_fill_gradient2(low = "blue", mid = "white",
                              high = "red", midpoint = 0, name = "kWh")
    }else{
      p2<-p1+scale_fill_gradientn(colours=c("black","yellow","red"), name = "kWh")
    }
    p2

  })
  
  output$plot5 <- renderPlot({
    dat.s<-dats.prep()
    
    allData.l<-melt(dat.s)
    names(allData.l)<-c("time","appart","energy")
    
    allData.l<-rbind(allData.l,data.frame(time=row.names(dat.s),appart=rep(NA,nrow(dat.s)),energy=meanApp))
    
    time2<-as.POSIXct(allData.l$time)
    allData.l$index<-time2
    
    allData.l<-allData.l%>%
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
    
    levels(allData.l$season)<-c("Winter","Spring","Summer","Autumn")
    
    if(input$seasonId=="hour"){
      allData.agg<-allData.l%>%
        group_by(appart,hour)%>%
        summarise(energy=median(energy,na.rm=T))
      
      p1<-ggplot(allData.agg)+
        geom_path(aes(x=hour,y=energy, color=appart))+
        labs(x="time of day", y="kWh")+
        theme_minimal()+
        theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
        scale_color_discrete(na.value="black")
    }
    
    if(input$seasonId=="hourDay"){
      allData.agg<-allData.l%>%
        group_by(appart,hour,WdayTag)%>%
        summarise(energy=median(energy,na.rm=T))
      
      p1<-ggplot(allData.agg)+
        geom_path(aes(x=hour,y=energy, color=appart))+
        facet_wrap(.~WdayTag,nrow = 1)+
        labs(x="time of day", y="kWh")+
        theme_minimal()+
        theme(axis.text.x = element_text(colour="grey20",size=6,angle=90,hjust=.5,vjust=.5,face="plain"))+
        scale_color_discrete(na.value="black")
    }
    
    if(input$seasonId=="hourDaySeas"){
      allData.agg<-allData.l%>%
        group_by(appart,hour,WdayTag,season)%>%
        summarise(energy=median(energy,na.rm=T))
      
      p1<- ggplot(allData.agg)+
        geom_path(aes(x=hour,y=energy,color=appart))+
        facet_grid(season~WdayTag)+
        labs(x="time of day", y="kWh")+
        theme_minimal()+
        theme(axis.text.x = element_text(colour="grey20",
                                         size=6,angle=90,hjust=.5,
                                         vjust=.5,face="plain"),
              legend.position = "none")+
        scale_color_discrete(na.value="black")
    }
    p1
    # ggsave(p1,filename = "../../temp/seasonPlot.png",
    #        width = 25,height=15,units = "cm",dpi = 800)
  })
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}