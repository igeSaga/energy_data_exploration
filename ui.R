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
                                      h3("Hintergrund:"),
                                      h5("Im Sommer 2018 hat die HSLU T&A der Gruppe SAGA die Finanzierung von 40 Stunden Arbeitszeit für die Umsetzung eines Innovationsprojektes gesprochen.
                                         Das Innovationsprojekt ist im Themenbereich 'Explorative Analyse von Monitoringdaten' angesiedelt.
                                         Innerhalb der Projektgruppe wurde entschlossen, das Thema als interaktive Applikation umzusetzen."),
                                      h3("Explorative Analyse von Energie-Monitoring Daten:"),
                                      h5("Das Feld der Explorativen Datenanalyse ist weit und reicht von der ersten Datenvisualsierung
                                         bis hin zum unüberwachten (d.h. unsupervised) Erkennen von Mustern und zur automatisierten Entscheidungsfindung.
                                         Provokative Stimmen sprechen in diesem Zusammenhang vom Ende der Theorie 
                                         und von der Diktatur der Daten (z.B: Anderson in https://www.wired.com/2008/06/pb-theory/)."),
                                      h5("In diesem Projekt geht diese Metadiskussion aussenvor. Wir haben neben Klassifikation und Klustering,
                                         beide Themen bedienen sich Methoden des maschinellen Lernens, 
                                         den Forschungsbereich der 'Visual Analytics' als drittes wichtiges Thema im Bereich der Explorativen Datenanalyse identifiziert."),
                                      h5("Die Applikation soll es dem Betrachter von Energiedaten ermöglichen, spielerisch und auf unterschiedliche Art und Weise,
                                         Muster in den Daten zu erkennen und damit weiterführende Hypothesen zu generieren.
                                         Im Zentrum steht der Betrachter und seine Beobachtungsgabe. Wir haben keine Mechanismen implementiert, 
                                         welche die Mustererkennung vorwegnehmen und damit a priori eine Hypothese nahelegen."),
                                      h3("Energiedaten:"),
                                      h5("Wir beziehen uns auf Information zum Stromverbrauch von Wohungen. Der Stromverbrauch stammt von 70 Haushalten im Suurstoffi Areal. 
                                         Der Verbrauch wurd pro Haushalt im 15min Takt und über 2.5 Jahre gemessen (2013-2015). Für eine Performante darstellung der Daten, wurde der Stromverbrauch zu Stundenwerten zusammengefasst (kWh)."),
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
