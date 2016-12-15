#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(markdown)
navbarPage("Tareas",
           tabPanel("Tarea_1",
                    titlePanel("Función Inversa"),
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("num1","Número de simulaciones",1000),
                        numericInput("num2","Lambda",.2),
                        actionButton("go", "Generar"),
                        numericInput("num3","Número de cajones del histograma",50),
                        radioButtons("bonaj", label = h3("Bondad de Ajuste"),
                                     choices = list("Ji-Cuadrada" = 1, "Kolmogorov-Smirnov" = 2, "Gráfica" = 3), 
                                     selected = 1),
                        actionButton("bo", "Bondad de Ajuste"),
                        downloadButton('downloadData', 'Descarga de muestra')
                      ),
                      mainPanel(
                        plotOutput("hist"),
                        verbatimTextOutput("value")
                      )
                    )
           ),
           tabPanel("Tarea_2",
                    titlePanel("Monte Carlo"),
                    sidebarLayout(
                      sidebarPanel(
                        textInput("expresion1","Función a integrar","function(x) sin(x)"),
                        numericInput("a","Límite inferior de la integral",0),
                        numericInput("b","Límite superior de la integral",6.28),
                        sliderInput("alpha1","Nivel de confianza de los intervalos",min=0,max=1,.5),
                        sliderInput("alpha2","Nivel de confianza de los intervalos",min=0,max=1,.05),
                        actionButton("go2", "Generar")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Gráfica función a integrar", plotOutput("graf")), 
                          tabPanel("Gráfica estimación con intervalos de confianza", plotOutput("grafint")), 
                          tabPanel("Gráficas comparativa de modelos", plotOutput("grafcomp")),
                          tabPanel("Tabla de estimaciones y limites",verbatimTextOutput("Result"))
                        )
                      )
                    )
           ),
           tabPanel("Tarea_4",
                       titlePanel("Metropolis-Hasting (set up inicial)"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variablex", "Variable independiente X:",
                                       c("Peso" = "1",
                                         "Talla" = "2")),
                           selectInput("variabley", "Variable dependiente Y:",
                                       c("Talla" = "2",
                                         "Peso" = "1")),
                           fluidRow(
                           numericInput("med_a",label="Media parámetro a",0),
                           numericInput("sd_a",label = "Desviacion parámetro a",0),
                           numericInput("med_b",label="Media parámetro b",0),
                           numericInput("sd_b",label = "Desviacion parámetro b",0),
                           numericInput("min_sd",label="Valor mínomo parámetro s",0),
                           numericInput("max_sd",label = "Valor máximo parámetro s",0)
                       )),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Datos", DT::dataTableOutput("table")), 
                           tabPanel("Sactter Plot", plotOutput("graft4")), 
                           tabPanel("Gráficas densidades", plotOutput("graf_dens"))
                         )
                       )
                       )
),
tabPanel("Tarea_5",
         titlePanel("Metropolis-Hasting (EL MCMC)"),
         sidebarLayout(
           sidebarPanel(
             sliderInput("slider1", label = h4("Número de cadenas que se quiere simular"),
                         min = 10, max = 10000, value = 1000),
             sliderInput("slider2", label= h4("La longuitud de las cadenas"),
                         min = 0, max = 10, value = 3)
           ),
           mainPanel()
           )
         )
)
