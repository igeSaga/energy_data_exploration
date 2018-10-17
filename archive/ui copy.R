library(shiny)

codes<-read.csv("data/sensor_codes.csv")
codes<-codes[codes$type=="whg",]
codes.l<-paste("whg nr: ",1:nrow(codes)," (haus nr; ",codes$house,", / fl; ",codes$fl," / zim; ",codes$zimmer,")",sep="")

  fluidPage(title = "Variables affecting Syntactic Distance in Switzerland",
            
            
            tabsetPanel(
              tabPanel(title = "Projektinformation",
                       pageWithSidebar(
                         headerPanel("Visuelle und Explorative Analyse von Energiedaten"),
                         sidebarPanel(width=12,
                                      h4("Projekt:"),
                                      h6("..."),
                                      h4("Diese Applikation:"),
                                      h6("..."),
                                      h4("Daten:"),
                                      h6("Stromverbräuche von Haushalten im Suurstoffi Areal. Es wurde der Stromverbrauch pro Haushalt über ein Jahr (2013-2014) im 15min Takt gemessen."),
                                      h4("Autoren:"),
                                      h6("Programmierung: Curdin Derungs, Projektleitung: Nadège Vetterli / Thomas Schluck")
                                      ),
                         mainPanel(
                         )
                       )
              ),
              
              
              tabPanel(title = "Visuelle Analyse (Einzelhaushalt)",
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
                       ),
              
              
              tabPanel(title = "Mustererkennung (Haushaltvergleich)",
                       fluidRow(
                         column(width = 12, class = "well",
                                fluidRow(
                                  column(width=4,
                                         selectInput(width = 400,inputId = "houseIds", h4("select an appartement"), choices = codes.l, multiple=T)
                                  ),
                                  column(width=4,
                                         selectInput(width = 400,inputId = "groupId",h4("select grouping"),choices = c("No grouping" = "no",
                                                                                                                        "Year" = "yr",
                                                                                                                        "Month" = "mn",
                                                                                                                        "Week Day" = "wd"))
                                  ),
                                  actionButton("go", "Go")
                                ),
                                h4("detailed zoom-in"),
                                fluidRow(
                                         plotOutput("plot5", height = 600)
                                )
                         )
                       )
              )
            )
)
