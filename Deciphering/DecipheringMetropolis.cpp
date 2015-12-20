#include <Rcpp.h>
using namespace Rcpp;


/* Funcion Freq 
 * Parametros: 
 * ngrams:  Entero. Numero de caracteres diferentes en el texto mas uno (espacio)
 * text:    Rcpp::CharacterVector. Texto para extraer frecuencias
 *
 * Regresa: Rcpp::NumericMatrix. Una matriz con las frecuencias de pares ordenados de letras
 *          y pares (espacio-letra)
 */
   
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


/* Funcion newFreq 
 * Parametros: 
 * ngrams:  Entero. Numero de caracteres diferentes en el texto mas uno (espacio)
 * text:    List(Rcpp::CharacterVector). Lista con textos para extraer frecuencias. 
 *          Se usa para generar matrices de frecuencias de libros o conjuntos de textos.
 *
 * Regresa: Rcpp::NumericMatrix. Una matriz con las frecuencias de pares ordenados de letras
 *          y pares (espacio-letra)
 */

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

    while (string[c] != '\0' && string[c+1] != '\0')
    {
      /** Consideramos solo caracteres 'a' a 'z'
       ignoramos los demas */

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


/* Funcion ciDecipher
 * Parametros: 
 * cipher:  Rcpp::IntegerVector. Llave que se empleara para cifrar/decifrar una cadena
 * text:    Rcpp::CharacterVector. Texto a cifrar/decifrar
 *          
 *
 * Regresa: Rcpp::CharacterVector. Una cadena a la que se le aplica la llave ingresada
 */
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



/* Funcion proposal
 * Parametros: 
 * cipher:  Rcpp::IntegerVector. Llave que se empleara para cifrar/decifrar una cadena
 * jump:    Entero. Numero de cambios aleatorios, que se realizaran para generar una nueva
 *          llave.
 *          
 *
 * Regresa: Rcpp::IntegerVector. Una nueva llave propuesta a partir de la llave ingresada
 */
// [[Rcpp::export]]
IntegerVector proposal(IntegerVector theta, int jump){
  int nparam = theta.size();

  for (int i=0; i<jump; i++){
    int ind=floor(R::runif(0,1)*nparam);
    int val=floor(R::runif(0,1)*nparam);

    //intercambiamos el valor anterior por el nuevo
    int ant=theta[ind];
    theta[ind]=val;
    for (int j=0;j<nparam;j++){
      if (theta[j]==val && j!=ind){
        theta[j]=ant;
      }
    }

    }
  return theta;
  }


/* Funcion objdens
 * Parametros: 
 * data:          Rcpp::NumericMatrix. Matriz de frecuencias del texto a decifrar
 * ref:           Rcpp::NumericMatrix. Matriz de frecuencias del texto de referencia
 * p:             Entero. Factor de exponenciacion
 * refLength:     Entero. Factor de escalamiento para la referencia
 * dataLength:    Entero. Factor de escalamiento para el texto
 *          
 *
 * Regresa: double. Score (Loglikelihood) de ajuste de la cadena asociada a la matriz data
 *          con referencia al texto de referencia representado en la matriz de frecuencias
 *          ref con parametros de ajuste p refLength, dataLength (En el presente trabajo
 *          solo se uso el valor de ajuste de p)
 */
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
    }
  }


  return lkh;
}      


/* Funcion objdens
 * Parametros: 
 * nsim:          Entero. Numero de simulaciones
 * theta0:        Rcpp::IntegerVector. Llave inicial
 * objdens:       Funcion. Función de Score
 * proposal       Funcion. Función de propuesta
 * text:    Rcpp::CharacterVector. Texto a cifrar/decifrar
 * ref:           Rcpp::NumericMatrix. Matriz de frecuencias del texto de referencia
 * jump:          Entero. Numero de cambios aleatorios, que se realizaran para generar una nueva
 *                llave.
 * p:             Entero. Factor de exponenciacion
 * refLength:     Entero. Factor de escalamiento para la referencia
 * dataLength:    Entero. Factor de escalamiento para el texto
 *          
 *
 * Regresa: Lista que contiene
 *          "theta". Cadena con las llaves generadas en el algoritmo
 *          "rejections". Vector con el número de rechazos para cada iteración
 *          "maxIter" Posición en la que se encuentra la llave con el max score
 */
// [[Rcpp::export]]
List MHBayes(int nsim, IntegerVector theta0, Function objdens, Function proposal, 
             CharacterVector text, NumericMatrix ref, int jump, int p, int refLength, int dataLength){

  int nparam=theta0.size();
  int maxIter=0;
  double newScore,maxScore=0.0;
  NumericMatrix theta(nsim, nparam);  
  theta(0,_) = theta0;
  NumericMatrix data=Freq(nparam+1,text); 
  //Vector que guarda el número de rechazos antes de aceptar una propuesta
  NumericVector rejections(nsim);
  // logU para el criterio de Metropolis
  double logU;
  // accept tells wether a proposal is accepted, trials counts attemps before accepting
  bool accept=false;
  // trials max is the maxnumber of inner cycles in what follows, trial the counter
  int trials;
  int maxtrials=10000;

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

      if(logU < prop) { 
        accept = true;
        theta(i,_) = newtheta;
        // Si el score supera el max score, lo guardaremos como max score así como el 
        // indice de la cadena en que se encuentra
        if (newScore > maxScore) {
          maxScore=newScore;
          maxIter=i;
        }
        data=newData;
      } 
      trials++;
    }  
    rejections[i] = trials;
  }
  return List::create(Named("theta")  = theta, Named("rejections")  = rejections, Named("maxIter") =maxIter);
}



