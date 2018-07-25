library(ggplot2)
library(shiny)
library(reshape2)
library(dplyr)
library(xts)

load("../output/allData.Rdata")

function(input, output) {
  
  time<-as.POSIXct(as.character(rownames(allData)), format="%d.%m.%Y %H:%M", tz = "GMT")
  attributes(time)$tzone <- "Europe/Zurich"
  time<-time - 3600
  
  allData.s<-allData
  allData.s[allData==0]<-NA
  meanTime<-apply(allData.s,1,function(x) mean(x, na.rm=T))
  allData.rel<-allData-meanTime
  
  # qu.all<-quantile(allData,na.rm=T,probs = seq(0,1,by = 0.1))
  # qu.all.rel<-quantile(allData.rel,na.rm=T,probs = seq(0,1,by = 0.1))
  # 
  # lab.all<-paste(round(qu.all,2),round(qu.all[-1],2),sep="-")
  # lab.all<-lab.all[1:(length(lab.all)-1)]
  # 
  # lab.all.rel<-paste(round(qu.all.rel,2),round(qu.all.rel[-1],2),sep="-")
  # lab.all.rel<-lab.all.rel[1:(length(lab.all.rel)-1)]
  
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
  
  # dat.reclass <- reactive({
  #   dat<-dat.prep()
  #   rec<-cut(dat,breaks = qu.all, labels = lab.all)
  #   if(input$relAbsId=="rel"){
  #     rec<-cut(dat,breaks = qu.all.rel, labels = lab.all.rel)
  #   }
  #   names(rec)<-names(dat)
  #   rec
  # })
  
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
    
    lo<-3000
    if(length(time.s<3000)){
      lo<-length(time.s)
    }
    
    time.s<-time.s[seq(from=1, to=length(time.s), length.out = lo)]
    dat.s<-dat.s[seq(from=1, to=length(time.s), length.out = lo)]
    
    dat.s<-data.frame(time=time.s,kWh=dat.s)
    
    ggplot(dat.s, aes(x = time.s, y = kWh)) +
      geom_path()+
      theme_minimal()+
      ylab("kWh/15min")
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
                              high = "red", midpoint = 0, name = "kWh/15min")
    }else{
      p2<-p1+scale_fill_gradientn(colours=c("black","yellow","red"), name = "kWh/15min")
    }
    p2

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