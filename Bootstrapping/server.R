
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
library(ggplot2)
setwd(".")
source('FuncionesBootstrapping.R',local = TRUE)
#source(gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb')))
shinyServer(function(input, output) {
  #Grafica que plotea la función a integrar con las funciones de densidad
  #usadas para Importance Sampling
  output$densPlot <- renderPlot({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data<-as.data.frame(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                                      quote=input$quote))
    
    X<-sample(data[,input$obs],input$nmuestra)
    n<-1    
    if("normal" %in% unlist(input$tipo)) { n<-n+1 }
    if("mc" %in% unlist(input$tipo)) { n<-n+1 }
    if("nested" %in% unlist(input$tipo)) { n<-n+1 }
    #creamos la estructura para 3 opciones de bootstrapping
    par(mfrow=n2mfrow(n))
    #
    if("mc" %in% unlist(input$tipo)) { 
      Fb_MC<-bootstrapMC(input$nsims,input$nmuestra,data[,input$obs])
      #Fb_MC<-bootstrapMC(input$nsims,length(X),X)
      #distribucion de la media
      d_mean<-sapply(Fb_MC,mean)
      media_MC<-mean(d_mean)
      #distribucion de la ds
      d_ds<-sapply(Fb_MC,ds)
      ds_MC<-mean(d_ds)
      #parametrizamos la Hipótesis Nula
      F_H0<-rnorm(1000,media_MC,ds_MC)
      hist_H0<-hist(F_H0, breaks=20,plot=TRUE,main="Hipótesis Nula (Bootstrap MC)")
      #d_X2<-sapply(Fb_MC,X2,media_MC,ds_MC,hist_H0$breaks,hist_H0$counts)
      #X2_MC<-mean(d_X2)
      d_Sh<-sapply(Fb_MC,Sh)
      Sh_MC<-mean(d_Sh)
      mediaH0<-sprintf("Media: %2.05f",media_MC)
      DSH0<-sprintf("DS: %2.05f",ds_MC)
      legend("topright", c( mediaH0,DSH0))
      texto<-sprintf("p_value: %1.16f",Sh_MC)
      mtext(texto,side=1)
      
    }
    if("nested" %in% unlist(input$tipo)) { 
      dm_Fb_NMC<-nestedBootstrap(bootstrapMC(input$nsims,length(X),X),mean)
      #distribucion de la media
      media_NMC<-mean(dm_Fb_NMC)
      #distribucion de la ds
      ds_Fb_NMC<-nestedBootstrap(bootstrapMC(input$nsims,length(X),X),ds)
      ds_NMC<-mean(ds_Fb_NMC)
      #parametrizamos la Hipótesis Nula
      F_H0<-rnorm(1000,media_NMC,ds_NMC)
      hist_H0<-hist(F_H0, breaks=20,plot=TRUE,main="Hipótesis Nula (Bootstrap MC)")
      ds_X2_NMC<-nestedBootstrap(bootstrapMC(input$nsims,length(X),X),X2)
      X2_MC<-mean(ds_X2_NMC)
      legend("topright", c("Media:",media_MC,"DS:",ds_MC))
      mtext(cat("pValue:",X2_MC),side=1)
      
    }
    #Graficamos el histograma de los datos reales observados
    hist(data[,input$obs],main = "Data")
    
  })
  
  
  output$contenido <- renderDataTable({
    
    #No se usa
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data<-as.data.frame(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote))
    data
  })
  


})
