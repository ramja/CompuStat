
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Pruebas de Hipótesis (Bootstrapping) - Prueba de Normalidad -"),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Seleccione el archivo',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separador',
                   c(Coma=',',
                     Puntoycoma=';',
                     Tab='\t'),
                   ','),
      numericInput("obs", "Columna a evaluar:", 1,
                   min = 1, max = 100),
      sliderInput("nmuestra",
                  "Tamaño de la Muestra Inicial",
                  min = 3,
                  max = 100,
                  value = 10
      ),
      checkboxGroupInput(inputId = "tipo",
                         label = "Elegir la opción para Boostrap",
                         choices = list("MonteCarlo"="mc")
      ),
      conditionalPanel(condition = "input.tipo == 'mc'",
                       sliderInput("nsims",
                                    "Seleccionar el número de simulaciones",
                                    min = 1,
                                    max = 4000,
                                    value = 100
                       )),
      conditionalPanel(condition = "input.tipo == 'nested'",
                       sliderInput("pseudoX",
                                    "Seleccionar el número de pseudo muestras",
                                   min = 1,
                                   max = 10000,
                                   value = 100
                       ))
      

    ),
    

  
    mainPanel(
      tabsetPanel( 
        tabPanel("Contenido", dataTableOutput("contenido")),
        tabPanel("Comparación", plotOutput("densPlot"))
      )
  )
))
)
