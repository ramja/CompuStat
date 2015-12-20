
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
setwd(".")
source('FuncionesImportance.R',local = TRUE)

shinyServer(function(input, output) {
  #Grafica que plotea la función a integrar con las funciones de densidad
  #usadas para Importance Sampling
  output$densPlot <- renderPlot({
    


    #Elección de g
    
    #Genero el dominio de las funciones
    X <- seq(-.50,3,.1)
    #Funcion a integrar  f_m
    plot(X,f_m(X,input$m),type='l',col=1)
    #función exponencial truncada
    if("e02" %in% unlist(input$g)) {
    lines(X,exp_0_2(X,input$lamd),col=2)
    }
    #función de densidad de una beta
    if("b02" %in% unlist(input$g)) {
    lines(X,dens_beta_0_2(X,input$alpha,input$beta), col=3)
    }
    
  })
  
  output$compGraph <-  renderPlot({
    
    
    nsim<-input$nmaxsim/100
    #Elección de g
    nmax<-input$nmaxsim
    #Genero el dominio de las simulaciones
    X <- seq(1,input$nmaxsim,abs(nsim))
    
    #genero un vector de simulaciones Monte Carlo
    simMC<-aprox_MC(input$nmaxsim,nsim,input$m)
    #Funcion a integrar  f_m
    plot(X[1:min(length(X),length(simMC$int))],simMC$int[1:min(length(X),length(simMC$int))],type='l',col=1)
    #intervalos de confianza
    if(!("e02" %in% unlist(input$g)) && !("b02" %in% unlist(input$g))){ 
      lines(X[1:min(length(X),length(simMC$int))],simMC$confInf[1:min(length(X),length(simMC$int))],type='l',col=4)
      lines(X[1:min(length(X),length(simMC$int))],simMC$confSup[1:min(length(X),length(simMC$int))],type='l',col=4)
    }
    #Dibujo el valor real
    abline(h=Intf_m_0_2(input$m))
    #Si se va a comparar con la exponencial truncada
    if("e02" %in% unlist(input$g) && !is.null(input$lamd)) {
      simE02<-aprox_E02(input$nmaxsim,nsim,input$m,input$lamd)
      lines(X,simE02, type = 'l', col=2)
    }
    #Si se va a comparar con la beta
    if("b02" %in% unlist(input$g)) {
      simB<-aprox_B(input$nmaxsim,nsim,input$m,input$alpha,input$beta)
      lines(X[1:min(length(X),length(simB))],simB[1:min(length(X),length(simB))], type = 'l', col=3)
      
    }

    
  })
  


})
