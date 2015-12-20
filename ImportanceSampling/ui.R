
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Importance Sampling- fm(x) = m*exp(-m*x) "),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nmaxsim",
                  "Máximo número de simulaciones",
                  min = 100,
                  max = 100000,
                  value = 1000
      ),
      sliderInput("m",
                  "Seleccionar el parámetro m",
                  min = 0,
                  max = 10,
                  value = 2
      ),
      checkboxGroupInput(inputId = "g",
                         label = "Elegir función para Importance Sampling",
                         choices = list("e[0,2] Truncada" = "e02", "beta" = "b02")
      ),
      conditionalPanel(condition = "input.g == 'e02'",
                       sliderInput("lamd",
                                    "Seleccionar parametro Lambda para la exponencial truncada",
                                    min = 0.000000000000001,
                                    max = 10,
                                    value = 1
                       )),
      sliderInput("alpha",
                  "Seleccionar el parámetro Alpha para la función beta",
                  min = 0,
                  max = 30,
                  value = 5
      ),
      sliderInput("beta",
                  "Seleccionar el parámetro Beta para la función beta",
                  min = 0,
                  max = 20,
                  value = 1
      )
      

    ),
    

  
    mainPanel(
      tabsetPanel( 
        tabPanel("Selección de valores",  plotOutput("densPlot")),
        tabPanel("Comparación", plotOutput("compGraph"))
      )
  )
))
)
