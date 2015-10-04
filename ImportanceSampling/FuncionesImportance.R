options(digits = 8)
set.seed(1452345)
# nsim<-1000
# m<-2
# lamd<-1
#genero una uniforme [0,2]
#U2<-runif(nsim,0,2)
dist_exp_0_2<-function(x,l) (1-exp(-l*x))/(1-exp(-l*2))
#exponencial (lambda=l) truncada a[0,2]
exp_0_2<-function(x,l) (l*exp(-l*x))/(1-exp(-l*2))
#inversa exponencial truncada
exp_inv<-function(y,l) (1/l)*log(1/(1-(1-exp(-2*l))*y))
#funcion de densidad de una beta(alpha,beta)
# alpha<-2
# beta<-3
dens_beta_0_2<-function(x,alpha,beta) 1/beta(alpha,beta)*(x/2^(alpha-1)*(1-(x/2))^(beta-1))

#funcion a integrar
f_m<-function(x,m)  m*exp(-m*x)  #if (0 < x && x < 2) m*exp(-m*x) else 0 
# Y<-f_m(U2,2)
# hist(Y)

#Integral [0,2]
Intf_m_0_2<-function(m) 1-exp(-2*m)



#MonteCarlo Crudo
aprox_MC<-function(nmax,nsim,m){
  result=list()
  conf=list()
  for (i in  c(1:nmax/nsim)) {
    U2<-runif(nsim,0,2)
    Ph<-2*f_m(U2,m)   
    result[i]<-mean(Ph)
    conf[i]<-mean(Ph)-intConf(Ph)
    # print result[i]-Intf_m_0_2(m)
  }
  list("int"=result,"conf"=conf)
}



# hist(Ph )
# mean(Ph)



#Importance Sampling

#Exponencial truncada
aprox_E02<-function(nmax,nsim,m,lamd){
  result=list()
  error=list()
  for (i in  c(1:nmax/nsim)) {
    UE<-exp_inv(runif(nsim),lamd)
    Ph_ET<-f_m(UE,2)/exp_0_2(UE,lamd)
    result[i]<-mean(Ph_ET)
    # print result[i]-Intf_m_0_2(m)
  }
  result
}



##Beta
aprox_B<-function(nmax,nsim,m,alpha,beta){
  result=list()
  error=list()
  for (i in  c(1:nmax/nsim)) {
    B<-2*rbeta(nsim,alpha,beta)
    Ph_B<-2*f_m(B,2)/dens_beta_0_2(B,alpha,beta)
    result[i]<-mean(Ph_B)
    # print result[i]-Intf_m_0_2(m)
  }
  result
}

#Intervalos de confianza
intConf<-function(x) qnorm(.95,mean(x),sqrt(var(x)/length(x)))

# plot(X,f_m(X,1),type = 'l')
# plot(X,inv_exp(X,1))
# plot(X,dens_beta_0_2(X,alpha,beta),type='l', col=sample(rainbow(10)))

# smoothingSpline = smooth.spline(X, 1-exp_0_2(X,3))
# lines(smoothingSpline, color='red')
# options(show.error.messages = TRUE)
# for (i in X) {
#   try(exp_1_2(X))
# }
# X[581]

# exp_1_2(X[581])