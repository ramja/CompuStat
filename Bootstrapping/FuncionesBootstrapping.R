setwd(".")

vinos<-read.table("MLPTRN.txt")
X<-sample(vinos[,1],10)
#parametros
fun<-function(x) var(x)
nboot<-1000 #numero de remuestreo Bootstrap
ds<- function(x) sqrt(sum((x-mean(x))^2)/(length(x)-1)) 

X2<-function(x,mu,sd,breaks,counts){
 obs<-pnorm(breaks,mu,sd)  
 m<-matrix(c(counts,obs),byrow=TRUE,ncol = length(counts))
 test<-chisq.test(m)
 as.numeric(test$p.value)
}
chi<-X2(c(10,1,10),5,1,c(0,5,10),c(10,10,10))

Sh<-function(x){
  test<-shapiro.test(x)
  as.numeric(test$p.value)
}
sh<-Sh(c(1,10,5,10,10,10))

KS<-function(x,mu,sigma){
  test<-ks.test(x,"pnorm",mu,sigma)
  as.numeric(test$p.value)
}
ks<-KS(c(10,1,10,0,5,10,10,10,10),0,1)

bootstrapMC<-function(n.sims, sample.size, muestra){
  #Bootstrap-mc 
  resample<-list()
  
  for (i in 1:n.sims){
    resample[[i]]<-sample(muestra, sample.size, replace = TRUE)
  }
  return(resample)
}

#Testing bootstrapMC
d<-bootstrapMC(1000,3,X)

#Agrega un indice al principio de un vector de indices
agregaI<-function(n,lista){
  if (length(lista)==0) { return(list(n))} 
  for (i in 1:length(lista)){
    lista[[i]]<-c(n,lista[[i]])
  }
  
  lista
}

# Test
a<-list()
a[[1]]<-c(1,2,3)
a[[2]]<-c(2,3)
b<-agregaI(0,a)

# Genera una secuencia de indices para Bootstrap
subsample<-function (L,i){
  M<-list()
  if (i==0) { return(M)} 
  M<-c(M,agregaI(L[1],subsample(L,i-1)))
  if (length(L)==1) {return(M)}
  M<-c(M,subsample(L[2:length(L)],i))
  M
}
#Test
b<-subsample(c(1,2,3),3)

#regresa un conunto de pseudo muestras bootstrap
bootstrap<-function(muestra,tamaño) {
  b<-list()
  indices<-subsample(c(1:tamaño),tamaño)
  for (i in 1:length(indices)){
    b[[i]]<-muestra[indices[[i]]]
  }
  return(b)
}

#Test de #
c<-bootstrap(X,3)
# 


#Aplica el estadístico al conjunto de muestras o seudomuestras
R0<-function(X,fun){
  return(lapply(X,fun))
}
#Test
prueba<-R0(c,var)

#Bootstrap anidado
nestedBootstrap<-function(pseudoX,fun){
 R_1<-list()
 for (i in 1:length(pseudoX)){
   dim(pseudoX[[i]])<-c(1,length(pseudoX[[i]]))
   R_0<-apply(pseudoX[[i]],1,FUN=fun)
   XX<-bootstrap(pseudoX[[i]],length((pseudoX[[i]])))
   R_00<-R0(XX,fun)
   cont<-0
   for (j in 1:length(XX)){
     if(R_00[j]<R_0){
       cont=cont+1
     }
   }
   R_1[[i]]<-cont/length(XX)
     
 }
 return(R_1)
}


#Test
#n<-nestedBootstrap(bootstrapMC(50,50,X),var)
# nsam<-100
#qplot(n,geom = "histogram")
# RX<-list(nsam)
# RMean<-list(nsam)
# RVar<-list(nsam)
# for (i in nsam) {
#   RX[i]<-sample(X,5,replace = TRUE)
#   RMean[i]<-mean(R[i])
#   RVar[i]<-var(R[i])
# }
# hist(RMean)
  

