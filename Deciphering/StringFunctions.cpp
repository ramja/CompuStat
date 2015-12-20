#include <Rcpp.h>

using namespace Rcpp;

/* Funcion accuracy 
 * Parametros: 
 * text1:    Rcpp::CharacterVector. Texto para comparar
 * text2:    Rcpp::CharacterVector. Texto para comparar
 *
 * Regresa: double. Presicion de similitud entre las dos cadenas. 
 *                  Valores entre 0 y 1
 */
// [[Rcpp::export]]
double accuracy(CharacterVector text1, CharacterVector text2){
  std::string string1 = Rcpp::as<std::string>(text1);
  std::string string2 = Rcpp::as<std::string>(text2);
  
  // Medimos el nivel de coincidencia entre los dos textos (accuracy)

  int count=0;
  int c=0;
  int letters=0;
  while (string1[c] != '\0')
  {
    /** Consideramos solo caracteres 'a' a 'z'
     ignoramos los demas */
    if (string1[c] >= 'a' && string1[c] <= 'z'
          && 	string2[c] >= 'a' && string2[c] <= 'z'  ){
      letters++;
      if (string1[c] == string2[c] ){
          count++;
      }
    }
    c++;
  } 

  return (double)count/letters;
} 




