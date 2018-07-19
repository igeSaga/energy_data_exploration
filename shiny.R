library(ggplot2)
library(Cairo) 
library(shiny)
library(reshape2)


ui <- fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 12,
                    plotOutput("plot3", height = 300)
             )
           ),
           fluidRow(
             column(width = 12,
                    plotOutput("plot2", height = 100,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             )
           )
           
    )
    
  )
)



server <- function(input, output) {
  
  strom<-read.csv("../data/pv.csv")
  names(strom)[1]<-"time"
  
  #strom<-strom[seq(start=1,to = nrow(strom),by=100),]
  
  strom<-strom[,c(1,sample(2:ncol(strom),size=4,replace = F))]
  
  strom.w<-melt(strom, id.vars=c("time"))
  
  strom$time<-strptime(as.character(strom$time), format="%d.%m.%y %H:%M")
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(strom.w, aes(x = time, y = value, group=variable,color=variable)) +
      geom_line()
  })
  
  output$plot3 <- renderPlot({
    ggplot(strom.w, aes(x = time, y = value, group=variable,color=variable)) +
      geom_line() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
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

shinyApp(ui, server)