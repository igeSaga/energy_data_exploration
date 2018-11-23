#### Description
## Definition of the User Interface of the ShinyApp

#loading library
library(shiny)

#reading data
load("../externalData/allDataAggH_13_15.Rdata")
codes.l<-colnames(allData)

  fluidPage(title = "Variables affecting Syntactic Distance in Switzerland",
            
            
            tabsetPanel(
              tabPanel(title = "Projektinformation",
                       pageWithSidebar(
                         headerPanel("Visuelle und Explorative Analyse von Energiedaten"),
                         sidebarPanel(width=12,
                                      h3("Hintergrund:"),
                                      h5("Im Sommer 2018 hat die Hochschule Luzern Technik & Architektur der Gruppe 'Simulation und Analyse von Gebaeuden und Arealen' die Finanzierung von 40 Stunden Arbeitszeit zur Umsetzung eines Innovationsprojektes gesprochen.
                                         Das Innovationsprojekt ist im Themenbereich 'Explorative Analyse von Monitoringdaten' angesiedelt.
                                         Innerhalb der Projektgruppe wurde beschlossen, das Thema als interaktive Applikation umzusetzen."),
                                      h3("Explorative Analyse von Energie-Monitoring Daten:"),
                                      h5("Das Feld der Explorativen Datenanalyse ist weit und reicht von der ersten Datenvisualsierung
                                         bis hin zum unueberwachten (d.h. unsupervised) Erkennen von Mustern und zur automatisierten Entscheidungsfindung.
                                         Provokative Stimmen sprechen in diesem Zusammenhang vom Ende der Theorie (z.B: Anderson in https://www.wired.com/2008/06/pb-theory/)."),
                                      h5("In diesem Projekt geht diese Metadiskussion aussenvor. Wir haben den Forschungsbereich der 'Visual Analytics' als
                                         wichtiges Thema im Bereich der Explorativen Datenanalyse identifiziert."),
                                      h5("Die vorliegende Applikation soll es dem Betrachter von Energiedaten ermoeglichen, spielerisch und auf unterschiedliche Art und Weise,
                                         Muster in den Daten zu erkennen und damit weiterfuehrende Hypothesen zu generieren.
                                         Im Zentrum steht der Betrachter und seine Beobachtungsgabe. Wir haben darauf verzichtet, Mechanismen zu implementieren, 
                                         welche die Mustererkennung vorwegnehmen und damit a priori eine Hypothese nahelegen."),
                                      h3("Energiedaten:"),
                                      h5("Wir fokussieren in dieser Applikation auf Information zum Stromverbrauch von Wohungen. Der Stromverbrauch stammt von 70 Haushalten im Suurstoffi Areal. 
                                         Der Verbrauch wurde pro Haushalt, im 15min Takt und über 2.5 Jahre gemessen (2013-2015). Für eine performante Darstellung der Daten, wurde der Stromverbrauch zu Stundenwerten zusammengefasst (kWh)."),
                                      h3("Projektgruppe:"),
                                      h5("Programmierung / Umsetzung: Curdin Derungs"),
                                      h5("Projektleitung: Nadège Vetterli / Thomas Schluck")
                                      ),
                         mainPanel(
                         )
                       )
              ),
              
              
              tabPanel(title = "Stomverbrauch (Einzelhaushalt)",
                       fluidRow(
                         column(width = 12, class = "well",
                                fluidRow(
                                  column(width=4,
                                         selectInput(width = 400,inputId = "houseId",h4("Wählen Sie eine Wohnung aus:"),choices = codes.l)
                                  ),
                                  column(width=4,
                                         selectInput(width = 400,inputId = "relAbsId",h4("Absoluter Verbrauch oder Vergleichswerte?"),choices = c("absolute" = "abs","vergleich" = "rel")
                                                     )
                                         ),
                                  column(width=4,
                                         selectInput(width = 400,inputId = "aggregId",h4("Wählen Sie eine Aggregation:"),choices = c("Tag" = "d",
                                                                                                         "Woche" = "w",
                                                                                                         "Monat" = "m"))
                                         )
                                  ),
                                textOutput("selected_var"),
                                h4("Wählen Sie einen Ausschnitt Ihres Interessens aus:"),
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
                                h4("Detailansicht:"),
                                fluidRow(
                                  column(width = 12,
                                         plotOutput("plot3", height = 150)
                                         )
                                  ),
                                h4("Dichtekarte:"),
                                fluidRow(
                                  column(width = 12,
                                         plotOutput("plot4", height = 200)
                                         )
                                  )
                                )
                         )
                       ),
              
              
              tabPanel(title = "Periodizitäten (Haushaltvergleich)",
                       fluidRow(
                         column(width = 12, class = "well",
                                fluidRow(
                                  column(width=4,
                                         selectInput(width = 400,inputId = "houseIds", h4("Wählen Sie eine oder mehrere Wohnungen aus"), choices = codes.l, multiple=T)
                                  ),
                                  column(width=4,
                                         selectInput(width = 400,inputId = "seasonId",h4("Wählen Sie mögliche Periodizitäten aus"),
                                                     choices = c("Tageszeit" = "hour",
                                                                 "Tageszeit & Wochentage" = "hourDay",
                                                                 "Tageszeit & Wochentage & Jahreszeit" = "hourDaySeas"))
                                  ),
                                  actionButton("go", "Visualisierung Anzeigen")
                                ),
                                h4("Interaktionsplot"),
                                fluidRow(
                                         plotOutput("plot5", height = 600)
                                )
                         )
                       )
              )
            )
)
