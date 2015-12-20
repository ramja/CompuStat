## Cargamos las librerias y funciones de desciframiento
# Nota: Es necesario establecer el directorio de trabajo como el directorio actual
# Pestaña Files/More/Set as Working Directory (En RStudio)
library(Rcpp)
library(tm)
sourceCpp("DecipheringMetropolis.cpp")
sourceCpp("StringFunctions.cpp")

## Preproceso de texto
## Consiste en tomar un texto de un archivo o una cadena de texto y: 
#   1. Eliminar los simbolos diferente a letras del abecedario (26 letras)
##  2. Convertir todos los caracteres a minusculas
##  3. Quitar espacios innecesarios (los espacios entre palabras se preservan)

# Creamos una funcion que preprocece el texto que usaremos como referencia
# en este caso el archivo olivertwist.txt que se encuentra en el directorio texts
preprocRef<- function(){
  #Preprocesamos la informacion que usaremos
  #Texto de Referencia
  cname <- file.path(".", "texts")   
  docs <- Corpus(DirSource(cname))  
  docs = tm_map(docs, removePunctuation, preserve_intra_word_dashes = FALSE)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, tolower)
  newFreq(27,docs[[1]])
}

# Ejecutamos el preproceso de texto de referencia
refa<-preprocRef()

#Texto a decodificar
## Este texto es muy difícil de decodificar por el tamaño que tiene, es decir el score
#  que genera solo se basa en un número pequeño de combinaciones de letras y su espacio
#  de comparación se reduce (lingitud 22 letras)
text1<-"Hello my name is Elena"
## Este es un texto que presenta un buen nivel de desciframiento debido a su longitud
## (1068 letras)
text2<-"Robert de Chesney was a medieval English Bishop of Lincoln. Educated at Oxford or Paris, Chesney was Archdeacon of Leicester before his election as bishop in December 1148. He served as a royal justice in Lincolnshire, and was an early patron of Thomas Becket. Although shown favour by King Stephen of England, including the right to a mint, Chesney was present at the coronation of King Henry II of England in 1154 and went on to serve Henry as a royal justice. In about 1160 Chesney became embroiled in a dispute with St Albans Abbey that was eventually settled when the abbey granted him land in return for his relinquishing any right to oversee the abbey. He was active in his diocese; more than 240 documents relating to his episcopal career survive. They show him mediating disputes between religious houses and granting exemptions and rights in his diocese. Chesney bought a house in London to serve as an episcopal residence, constructed an episcopal palace in Lincoln, and founded a religious house outside the city. He died in December 1166 and was buried in"
## Este es el texto con que se hizo el analisis para el reporte (longitud 440 letras)
text3<-"Ice hockey is a contact team sport played on ice, usually in a rink, in which two teams of skaters use their sticks to shoot a vulcanized rubber puck into their opponent's net to score points. Ice hockey teams usually consist of four lines of three forwards, three pairs of defencemen, and two goaltenders. Normally, each team has five players who skate up and down the ice trying to take the puck and score a goal against the opposing team"

##Creamos una funcion que preprocece un texto para su encriptamiento-decriptamiento
preprocText<- function(text){
text <- Corpus(VectorSource(text))
text = tm_map(text, removePunctuation, preserve_intra_word_dashes = FALSE)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, tolower)
text <- tm_map(text, stripWhitespace) 
}

## Ejecutamos la funcion para el texto 2
text<-preprocText(text2)
## Generamos una llave aleatoria para cifrar el texto
init <- sample.int(26,replace = FALSE)-1
## Ciframos el texto seleccionado
a<-ciDecipher(init,text[[1]])

## ALGORITMO METROPOLIS HASTINGS

## Ajustamos los parametros
## Numero de iteraciones de Metropolis-Hastings
nsim <- 70000
## Creamos una llave inicial para ejecutar el algoritmo
init <- sample.int(26,replace = FALSE)-1


  #tiempo de computo
  ptm <- proc.time()
  ## Ejecutamos el algoritmo
  ## valor p=300 de acorde al reporte es el mejor valor
  #  a=texto a decifrar
  #  refa=texto de referencia
  i<-MHBayes(nsim, init, objdens, proposal, a, refa,1,300,1,1)
  spent<-proc.time() - ptm
  print("Tiempo de procesador empleado")
  print("-----------------------------")
  print(spent)

  #Medimos la precision de la decodificación
  d<-ciDecipher(i$theta[dim(i$theta)[1],],a)
  acc<-accuracy(d,text$content[[1]])
  print("      Accuracy               ")
  print("-----------------------------")
  print(acc)
  #medimos el max score
  print(" Accuracy (Max Score)        ")
  print("-----------------------------")  
  best<-ciDecipher(i$theta[i$maxIter,],a)
  acc<-accuracy(best,text$content[[1]])
  print(acc)



  ### 1) REJECTION RATES
  rejections <- i$rejections[-1]
  trials <- rejections + 1
  rej.rate <- cumsum(rejections)/cumsum(trials)
  plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
  plot(trials[-1], type="l", main="Number of trials")

  ## Guardamos la ultima sesión
  save(list=ls(), file="cipherN.RData")
