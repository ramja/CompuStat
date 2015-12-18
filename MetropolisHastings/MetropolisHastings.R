library(Rcpp)

# DATA
data(iris)





splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}


datasets<-splitdf(iris)
# OUR DATA WILL BE IN MATRIX FORM!!! MANDATORY!!!! IF YOU DON'T LIKE IT
# YOU CAN USE A WRAPPER FUNCTION IN R THAT CALLS THE C FUNCTIONS AND TRANSFORMS
# A VECTOR OR A DATAFRAME INTO A NUMERICAL MATRIX. BUT THIS WAY WE CAN 
# GENERALISE THIS EXAMPLE IT TO REGRESSIONS AND LARGE DATA!
X_train <- matrix(c(datasets$trainset$Sepal.Length,datasets$trainset$Sepal.Width,datasets$trainset$Petal.Length), ncol=3)
y_train <- matrix(datasets$trainset$Petal.Width, ncol = 1)
X_test<- matrix(c(datasets$testset$Sepal.Length,datasets$testset$Sepal.Width,datasets$testset$Petal.Length), ncol=3)
y_test <- matrix(datasets$testset$Petal.Width, ncol = 1)


# 2) Proposal: random walk in the same dimension as the number of parameters
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
proposal(c(1,2),0.1)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

# 1)
cppFunction('
  double objdens(NumericMatrix data,NumericMatrix obj, NumericVector theta){
    double lkh, logprior;
    int m=data.nrow();
    int n=theta.size()-2;
    //printf("n%d-m%d",n,m);
    NumericMatrix X(m,theta.size()-1);
    NumericVector y(m);
    NumericVector res(m);
    NumericVector aux(n);
    double sigma=theta[n+1];
    //printf("sigma%f",sigma);
    X = data(_,Range(0,theta.size()-2)); // In this example is redundant but helps to generalise
    //y = obj; // In this example is redundant but helps to generalise
    // Compute loglikelihood
    for (int j=0;j<m;j++){
      aux=X(j,_)*theta[seq(1,n)];
      res[j] = std::accumulate(aux.begin(),aux.end(),0.0)+theta[0];
      //res=X*theta(Range(1,n))+theta(0);
      //printf("aux%f-%f_%f-%f-res%f",aux[0],aux[1],aux[2],res[j]);
    }
    
    
    lkh=0;
    for (int i=0; i<m; i++){
      lkh += -.5*pow((obj[i,1]-res[0])/sigma,2)-log(sigma);
    }
    // Compute logpriorX
    logprior = 0.0;
    for (int k=1; k<n; k++){
      logprior += R::dnorm(theta[k], 3.0,.5, true);
    }
    logprior +=  R::dgamma(sigma, 5.0, 1.0/40.0, true);
    // Log of target density

    return lkh + logprior;
}')
objdens(X_train[1:3,],y_train, c(4,2,3,5,6))


# 3) METROPOLIS

sourceCpp("BayesianMH.cpp")

nsim <- 1000
init <- c(4,2,3,5,6)
pba<-MHBayes(20, init, objdens, proposal, X_train,y_train,.1)
mh.samp <- MHBayes(nsim, init, objdens, proposal, X_train,y_train, .01)
estims <- mh.samp$theta