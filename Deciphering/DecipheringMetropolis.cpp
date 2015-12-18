#include <Rcpp.h>
//#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix Freq(int ngrams, CharacterVector text){
  std::string string = Rcpp::as<std::string>(text);
  // Matriz con las frecuencias para cada bigrama
  NumericMatrix count(ngrams, ngrams);  
  std::fill( count.begin(),count.end(), 1 ) ;
  int c=0;
  while (string[c] != '\0' && string[c+1] != '\0')
  {
    /** Consideramos solo caracteres 'a' a 'z'
    ignoramos los demas */
    //printf("val%i",(string[c]-'a'));
    if (string[c] >= 'a' && string[c] <= 'z'
          && 	string[c+1] >= 'a' && string[c+1] <= 'z'  ){
      
      count(string[c]-'a',string[c+1]-'a')++;
    } else if (string[c] >= 'a' && string[c] <= 'z'
                 && 	string[c+1] == ' '){
      
      count(string[c]-'a',ngrams-1)++;
    } else if (string[c] == ' '
                 && 	string[c+1] >= 'a' && string[c+1] <= 'z'  ){
      
      count(ngrams-1,string[c+1]-'a')++;
    }
    
    c++;
  } 
  return  count;
} 

// [[Rcpp::export]]
NumericMatrix newFreq(int ngrams, List text){

  // Matriz con las frecuencias para cada bigrama
  int n = text.size();
  printf("n:%li",text.size());
  NumericMatrix count(ngrams, ngrams);  
  std::fill( count.begin(),count.end(), 1 ) ;
  int r=0;
  while (r<n){
    std::string string = Rcpp::as<std::string>(text[r]);
    int c=0;
    //printf("n:%d",r);
    while (string[c] != '\0' && string[c+1] != '\0')
    {
      /** Consideramos solo caracteres 'a' a 'z'
       ignoramos los demas */
      //printf("val%i",(string[c]-'a'));
      if (string[c] >= 'a' && string[c] <= 'z'
            && 	string[c+1] >= 'a' && string[c+1] <= 'z'  ){
        
            count(string[c]-'a',string[c+1]-'a')++;
      } else if (string[c] >= 'a' && string[c] <= 'z'
            && 	string[c+1] == ' '){
        
            count(string[c]-'a',ngrams-1)++;
      } else if (string[c] == ' '
            && 	string[c+1] >= 'a' && string[c+1] <= 'z'  ){
            
            count(ngrams-1,string[c+1]-'a')++;
      }
        
      
      c++;
    }
    r++;
  }
  return  count;
} 

// [[Rcpp::export]]
CharacterVector ciDecipher(IntegerVector cipher, CharacterVector text){
  std::string string = Rcpp::as<std::string>(text);
  
  int c=0;
  while (string[c] != '\0' )
  {
    /** Consideramos solo caracteres 'a' a 'z'
     ignoramos los demas */
    if (string[c]!=' '){
      string[c]=cipher[string[c]-'a']+'a';
    }
    c++;
  } 
  
  return string;
}
//Proposal: random walk in the same dimension as the number of parameters
// [[Rcpp::export]]
IntegerVector proposal(IntegerVector theta, int jump){
  int nparam = theta.size();

  for (int i=0; i<jump; i++){
    int ind=floor(R::runif(0,1)*nparam);
    int val=floor(R::runif(0,1)*nparam);
  //  printf("%d,%d-%d\n",newSubs(i),newSubs(i+1),theta[newSubs(i)]); 
  //intercambiamos el valor anterior por el nuevo
    int ant=theta[ind];
    theta[ind]=val;
    for (int j=0;j<nparam;j++){
      if (theta[j]==val && j!=ind){
        theta[j]=ant;
      }
    }
    //printf("%d,%d-%d\n",ind,val,ant);
    }
  return theta;
  }

// [[Rcpp::export]]
double objdens(NumericMatrix data,NumericMatrix ref,int p, int refLength, int dataLength){

  unsigned int ncol = data.ncol();
  unsigned int nrow = data.nrow();
  NumericMatrix X( nrow,ncol);
  double scale=log(refLength/dataLength);
  int counter = 0;
  double lkh=0;
  for (unsigned int j=0; j<ncol; j++) {
    for (unsigned int i=0; i<nrow; i++)  {
      lkh += data(i,j)*(log(ref(i,j))+log(refLength));
      //printf("-%f\n",lkh);
    }
  }
  
  //double s = Rcpp::sum(X);
  //printf("lkh:%f",lkh);
  
  return lkh;
}      

// [[Rcpp::export]]
List MHBayes(int nsim, IntegerVector theta0, Function objdens, Function proposal, 
             CharacterVector text, NumericMatrix ref, int jump, int p, int refLength, int dataLength){
  // theta will contain the output, one column pero parameter, row per simulation
  int nparam=theta0.size();
  int maxIter=0;
  double newScore,maxScore=0.0;
  NumericMatrix theta(nsim, nparam);  
  theta(0,_) = theta0;
  NumericMatrix data=Freq(nparam+1,text); 
  // X will save proposals, Rej will save number of rejection rates=(trials-1)/trials
  NumericVector X(nparam);
  NumericVector rejections(nsim);
  // logU is for the test
  double logU;
  // accept tells wether a proposal is accepted, trials counts attemps before accepting
  bool accept=false;
  // trials max is the maxnumber of inner cycles in what follows, trial the counter
  int trials;
  int maxtrials=10000;
  //double jump = .1;
  // outer cycle: sim n jumps
  for (int i=1; i<nsim; i++){
    // inner cycle: repeat until accepting
    trials = 0;
    accept = false;
    while (!accept && trials<maxtrials){
      IntegerVector newtheta  = (proposal(theta(i-1,_),jump));
      logU = log(R::runif(0,1));
      CharacterVector newText=ciDecipher(newtheta,text);
      NumericMatrix newData = Freq(nparam+1,newText);
      // the minus is since we used LOGS!!!!!
      newScore=as<double>(objdens(newData,ref,p, refLength, dataLength));
      double prop=p*(newScore-as<double>(objdens(data,ref,p, refLength, dataLength)));
      //printf("val:%fprop:%f",logU,prop);
      if(logU < prop) { 
        accept = true;
        theta(i,_) = newtheta;
        if (newScore > maxScore) {
          maxScore=newScore;
          maxIter=i;
          //printf("MS:%f-%d",maxScore,i);
        }
        data=newData;
      } 
      trials++;
    }  
    rejections[i] = trials;
  }
  return List::create(Named("theta")  = theta, Named("rejections")  = rejections, Named("maxIter") =maxIter);
}



