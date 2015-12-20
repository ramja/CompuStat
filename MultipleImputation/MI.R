library(foreign)
library(mvtnorm)


# Cargamos nuestras datos del archivo datos_politicos.dta
# Nota: Es necesario establecer el directorio de trabajo como el directorio actual
# Pestaña Files/More/Set as Working Directory (En RStudio)


table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
año <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO A?O PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==año, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

## Nuestra variable de interés
Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
# VARIABLES ECON?MICAS EXPLICATIVAS
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") 
X<-subset(data,select=list.depend)
for(j in 1:ncol(X)) X[,j]<-as.numeric(X[,j])

nrows<-nrow(X)
ncols<- ncol(X)
## Numero de modelos Bootstrap que realizaremos
m=5
tol<-1e-3
res<-list()
imputed.sets<-list()
pred.success<-numeric(m)
  
## Creamos m modelos
for (rep in 1:m){
  ## Hacemos un sampleo Bootstrap
  samp<-sample(1:nrows,nrows,replace=TRUE)
  Xb<-X[samp,]
  
  ## Tomamos los valores faltantes
  M<-is.na(Xb)
  ## Calculo la media y la ds
  sigma<- cov(Xb[complete.cases(Xb),])
  sd.vec<- sqrt(diag(sigma))
  mu<- apply(Xb[complete.cases(Xb),],2,mean)
  ## Damos un valor inicial a los valores faltantes
  for(i in 1:nrows) for(j in 1:ncols) if (M[i,j]) Xb[i,j] <- rnorm(1, mu[j], sd.vec[j])
  ## Calculomos la logverosimilitud completa
  logv<-sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,sigma))))
  # Iteramos hasta la convergencia
  iter<-1
  repeat{ 
    # Valor actual de la verosimilitud
    # iteraciones por variable
    for(j in 1:ncol(Xb)){
      ind<-as.matrix(Xb[,j],ncol=1)
      dep<-as.matrix(Xb[,-j])
      mod<-lm(ind ~dep)
      pred<-predict(mod)
      ## Inferimos los valores faltantes con la regresion lineal
      Xb[M[,j],j]<- pred[M[,j]]
    }
    
    # Nueva martriz de covarianzas
    sigma<-cov(Xb)
    mu<-apply(Xb,2,mean)
    ## Logverosimilitud completa
    logv[iter+1]<-sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,sigma))))
    if(abs(logv[iter+1]-logv[iter]<tol)) break
    iter<-iter+1  
  }
  print(paste("   -iteraciones totales:", iter))
  imputed.sets[[rep]]<-Xb
  # GRAFICA
  ## Graficamos para visualizar la convergencia de la logverosimilitud
  plot(logv[-(1:3)], type="l", col="blue", main=paste("Bootstrap", rep))
  #Modelo
  data.full<-data.frame(Y[samp], Xb)
  names(data.full)[1]<-"Y"
  ## Hcemos predicciones de la variable de interes
  res[[rep]]<-glm(Y ~ ., data=data.full, family="binomial") #regresion log
  ## Promediamos los resultados
  guess<- round(predict(res[[rep]],type="response"))
  pred.success[rep]<- sum(guess==data.full$Y)/nrows
    
}
## Corregimos el sesgo
#  Pooling
beta.all <- matrix(0, nrow=ncols, ncol=m)
for (j in 1:m){
  beta.all[,rep] <- coef(res[[rep]])[-1]
}
## Obtenemos los promedios de la base de datos completa
beta.estims <- apply(beta.all,2,mean)

## Estimamaos la varianza de cada iteracion bootstrap
beta.var.within <- numeric(ncols)
for (rep in 1:m){ 
  beta.var.within <- beta.var.within + (summary(res[[rep]])$coefficients[,2][-1])^2/m
}
## Estimamos la varianza entre iteraciones
beta.var.between <- apply(beta.all,1,var)
## Calculamos la varianza total
beta.var <- beta.var.within + (1+1/m)*beta.var.between

#Z-VALUES
table <-data.frame(beta=beta.estims, sd=sqrt(beta.var))
round(table,5)


save(list=ls(), file="MIdata.RData")