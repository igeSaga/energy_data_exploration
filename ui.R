library(shiny)

codes<-read.csv("../data/sensor_codes.csv")
codes<-codes[codes$type=="whg",]
codes.l<-paste("whg nr: ",1:nrow(codes)," (haus nr; ",codes$house,", / fl; ",codes$fl," / zim; ",codes$zimmer,")",sep="")

fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           fluidRow(
             column(width=4,
             selectInput(width = 400,inputId = "relAbsId",h4("absolute measures or deviation to mean"),choices = c("absolute" = "abs","deviation" = "rel")
                         )
             ),
             column(width=4,
                    selectInput(width = 400,inputId = "aggregId",h4("select an aggregation"),choices = c("Day" = "d",
                                                                                                         "Week" = "w",
                                                                                                         "Month" = "m"))
             ),
             column(width=4,
                selectInput(width = 400,inputId = "houseId",h4("select an appartement"),choices = codes.l)
             )
           ),
           textOutput("selected_var"),
           h4("select a temporal extent of interest"),
           fluidRow(
             column(width = 12,
                    plotOutput("plot2", height = 80,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             )
           ),
           h4("detailed zoom-in"),
           fluidRow(
             column(width = 12,
                    plotOutput("plot3", height = 150)
             )
           ),
           h4("heat-map"),
           fluidRow(
             column(width = 12,
                    plotOutput("plot4", height = 200)
             )
           )
           
    )
    
  )
)
