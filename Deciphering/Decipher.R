library(Rcpp)
library(tm)
sourceCpp("DecipheringMetropolis.cpp")
sourceCpp("StringFunctions.cpp")

#Preprocesamos la informacion que usaremos
#Texto de Referencia
cname <- file.path(".", "texts")   
docs <- Corpus(DirSource(cname))  
docs = tm_map(docs, removePunctuation, preserve_intra_word_dashes = FALSE)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
refa<-newFreq(27,docs[[1]])

#Texto a decodificar
text<-"Ice hockey is a contact team sport played on ice, usually in a rink, in which two teams of skaters use their sticks to shoot a vulcanized rubber puck into their opponent's net to score points. Ice hockey teams usually consist of four lines of three forwards, three pairs of defencemen, and two goaltenders. Normally, each team has five players who skate up and down the ice trying to take the puck and score a goal against the opposing team"
text <- Corpus(VectorSource(text))
text = tm_map(text, removePunctuation, preserve_intra_word_dashes = FALSE)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, tolower)
text <- tm_map(text, stripWhitespace) 
t<-newFreq(27,a)text[[1]])


##Ajustamos los parametros
nsim <- 10000
init <- seq(25,0)
orig <- text[[1]]
a<-ciDecipher(init,text[[1]])
init <- seq(0,25)
par<-c(1,10,100,1000)

  #tiempo de computo
  ptm <- proc.time()
  i<-MHBayes(nsim, init, objdens, proposal, a, refa,1,300,1,1)
  spent<-proc.time() - ptm
  print(spent)

  #Medimos la precision de la decodificación
  d<-ciDecipher(i$theta[dim(i$theta)[1],],a)
  acc<-accuracy(d,text$content[[1]])
  print(acc)
  #medimos el max score
  best<-ciDecipher(i$theta[i$maxIter,],a)
  acc<-accuracy(best,text$content[[1]])
  print(acc)



  ### 1) REJECTION RATES
  rejections <- i$rejections[-1]
  trials <- rejections + 1
  rej.rate <- cumsum(rejections)/cumsum(trials)
  plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
  plot(trials[-1], type="l", main="Number of trials")

save(list=ls(), file="cipherN.RData")
