library(Rcpp)

# DATA
data(iris)




#  Tomamos el conjunto de datos (iris) y dividimos el conjunto en un conjunto
#  de entrenamiento y un conjunto de pruebas. Para esto definimos una funcion
#  que realice la separacion de los datos de forma aleatoria
#
#  Funcion splitdf
#  Entradas 
#  dataframe: data.frame . Dataframe con el conjunto de datos (variables dependientes y valor esperado)
#  seed: numeric. Semilla para fijar el sample con el que se hara la division del conjunto de datos
#  percentage: numeric. Porcentaje del conjunto total para el set de entrenamiento
#  Salidas
#  Lista con
#  trainset: data.frame. Conjunto de entrenamiento
#  testset: data.frame. Conjunto de pruebas
splitdf <- function(dataframe, seed=NULL, percentage=.5) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*percentage))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

# Dividimos nuestro conjunto de datos
# con 80% para entrenamiento 20% para pruebas
datasets<-splitdf(iris, percentage = .80)


# OUR DATA WILL BE IN MATRIX FORM!!! MANDATORY!!!! IF YOU DON'T LIKE IT
# YOU CAN USE A WRAPPER FUNCTION IN R THAT CALLS THE C FUNCTIONS AND TRANSFORMS
# A VECTOR OR A DATAFRAME INTO A NUMERICAL MATRIX. BUT THIS WAY WE CAN 
# GENERALISE THIS EXAMPLE IT TO REGRESSIONS AND LARGE DATA!

## Generamos nuestros conjuntos de entrenamiento, valores esperados de entrenamiento
## Igualente para set de pruebas
X_train <- matrix(c(datasets$trainset$Sepal.Length,datasets$trainset$Sepal.Width,datasets$trainset$Petal.Length), ncol=3)
y_train <- matrix(datasets$trainset$Petal.Width, ncol = 1)
X_test<- matrix(c(datasets$testset$Sepal.Length,datasets$testset$Sepal.Width,datasets$testset$Petal.Length), ncol=3)
y_test <- matrix(datasets$testset$Petal.Width, ncol = 1)

## Creamos una funcion para calcular el error cuadrático 
mse<- function(Y,X,betas){
  sum((Y-(X%*%betas[2:4]+betas[1]))**2) 
}
## Probamos la funcion para ver si funciona correctamente
mse(y_test[0:5,],X_test[0:5,],c(1,1,1,1))


# Definimos una funcion que de una funcion de densidad propuesta
# proposal: random walk in the same dimension as the number of parameters
cppFunction('
  NumericVector proposal(NumericVector theta, double jump){
    int nparam = theta.size();
    //double jump = .1; 
    NumericVector newtheta(nparam);
    for (int i=0; i<nparam; i++){
      newtheta[i] = R::rnorm(theta[i], jump);
    }
    return newtheta;
}')

## Probamos nuestra funcion propuesta para asegurarnos que funciona correctamente
proposal(c(1,2),0.1)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

# Nuestra funcion de densidad logaritmica
# Parametros
# data: NumericMatrix. Vector de muestras de variables independientes
# obj:  NumericMatrix. Vector de muestras de la variable dependiente
# theta: NumericVector. Vector de parametros.
cppFunction('
  double objdens(NumericMatrix data,NumericMatrix obj, NumericVector theta){
    /* m es el numero de muestras de nuestras variables                                    */
    int m=data.nrow();

    /* n es el numero de variables                                                         */
    int n=theta.size()-2;
    //printf("n%d-m%d",n,m);
    /* Definimos las variables que usaremos                                                */


    /* LogLikelihood y LogPrior                                                            */
    double lkh, logprior;
    /* Variables independientes y dependiente                                              */
    NumericMatrix X(m,n);
    NumericVector y(m);
    /* Variables auxialares                                                                */
    NumericVector res(m);
    NumericVector aux(n);
    /* Varianza                                                                            */
    double sigma=theta[n+1];
    //printf("sigma%f",sigma);

    /* X es nuestro vector de  variables independientes                                    */
    X = data(_,Range(0,n-1)); 

    /* y es nuestra variable dependiente                                                   */
    y = obj(_,0); 
    // Compute loglikelihood
    for (int j=0;j<m;j++){
      /* tomamos solo theta 1 a n (theta(0) es el intercepto)                              */
      aux=X(j,_)*theta[seq(1,n)];
      /* sumamos y añadimos el intercepto                                                  */
      res[j] = std::accumulate(aux.begin(),aux.end(),0.0)+theta[0];
      //res=X*theta(Range(1,n))+theta(0);
      //printf("aux%f-%f_%f-%f-res%f",aux[0],aux[1],aux[2],res[j]);
    }
    
    /*   Calculamos el loglikelihood                                                       */ 
    lkh=0;
    for (int i=0; i<m; i++){
      lkh += -.5*pow((y[i,1]-res[0])/sigma,2)-log(sigma);
    }

    /*   Calculamos el log de la distribucion inicial                                      */ 
    logprior = 0.0;
    for (int k=1; k<n; k++){
      logprior += R::dnorm(theta[k], 0,.1, true);
    }
    logprior +=  R::dgamma(sigma, 5.0, 1.0/40.0, true);

    /*   Regresamos log de la densidad objetivo = loglikelihood + logprior                 */
    return lkh + logprior;
}')

## Probamos nuestra funcion de densidad para asegurarnos que funciona correctamente
objdens(X_train,y_train, c(4,2,3,5,6))


# METROPOLIS
# Cargamos nuestras funciones con el algoritmo Metropolis-Hastings
# Nota: Es necesario establecer el directorio de trabajo como el directorio actual
# Pestaña Files/More/Set as Working Directory (En RStudio)
sourceCpp("BayesianMH.cpp")

## Numero de simulaciones que realizaremos 
nsim <- 1000

## Inicializamos nuestros parámetros
init <- c(1,0,0,0,1)

## Probamos nuestro algoritmo con 20 iteraciones para probar que funcione correctamente
pba<-MHBayes(20, init, objdens, proposal, X_train,y_train,.1)

## Ejecutamos Metropolis Hastings con parametro (jump = .00125 ~ pow(.013,3) ), seleccionado
## empiricamente mediante pruedas de 200 iteraciones con diferentes valores de un
## conjunto de valores en escala logaritmica (.1,.01,.001,.0001)
mh.samp <- MHBayes(nsim, init, objdens, proposal, X_train,y_train, .00125)

## Obtenemos la cadena del algoritmo con las distribuciones de nuestros parametros
estims <- mh.samp$theta

## Revisamos la taza de rechazos
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")

# eliminamos las primeras N instancias de las cadenas (burning). Recomendado el 10% del numero
# de iteraciones
burnin <- 100
estim <- estims[-(1:burnin), ]

## Revisamos si existe correlación en las cadenas generadas
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
acf(estims[ , 3])
acf(estims[ , 4]) 
acf(estims[ , 5])

## Si observamos correlacion en las cadenas generadoas podemos realizar un adelgazamiento
## de las cadenas. Recomendado empiricamene un 90%
thinning <- .9 # meaning we'll keep 75% of observations to reduce autocorrelation
# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]

## Volvemos a revisar la correlacion
acf(estims[ , 1])
acf(estims[ , 2]) 
acf(estims[ , 3])
acf(estims[ , 4]) 
acf(estims[ , 5])

#Estimamos la media de las distribuciones de cada uno de los parametros
beta0<-mean(estims[ ,1]) 
beta1<-mean(estims[ ,2])
beta2<-mean(estims[ ,3]) 
beta3<-mean(estims[ ,4]) 
sigma<-mean(estims[ ,5]) 

## Creamos un vector de parametros para la estimación
betas=c(beta0,beta1,beta2,beta3)

##Calculamos el error cuadratico en nuestro conjunto de pruebas
err<-mse(y_test,X_test,betas)





