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
                        actionButton("go", "Histograma"),
                        radioButtons("bonaj", label = h3("Bondad de Ajuste"),
                                     choices = list("Ji-Cuadrada" = 1, "Kolmogorov-Smirnov" = 2), 
                                     selected = 1),
                        actionButton("bo", "Bondad de Ajuste")
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
                        numericInput("numon1","Número de simulaciones",1000),
                        textInput("expresion1","Función a integrar","function(x) 2*x"),
                        numericInput("a","Límite inferior de la integral",0),
                        numericInput("b","Límite superior de la integral",1),
                        numericInput("alpha","Nivel de confianza de los intervalos",.5),
                        verbatimTextOutput("Result")
                      ),
                      mainPanel(
                        plotOutput("graf"),
                        plotOutput("grafint")
                      )
                    )
           )
)