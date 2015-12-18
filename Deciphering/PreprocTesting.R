library(tm)
library(Rcpp)
sourceCpp("DecipheringMetropolis.cpp")
cname <- file.path(".", "texts")   
docs <- Corpus(DirSource(cname))   

summary(docs)   



docs = tm_map(docs, removePunctuation, preserve_intra_word_dashes = FALSE)
#docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
#docs <- tm_map(docs, stripWhitespace) 
refa<-newFreq(27,docs[[1]])


text<-"Ice hockey is a contact team sport played on ice, usually in a rink, in which two teams of skaters use their sticks to shoot a vulcanized rubber puck into their opponent's net to score points. Ice hockey teams usually consist of four lines of three forwards, three pairs of defencemen, and two goaltenders. Normally, each team has five players who skate up and down the ice trying to take the puck and score a goal against the opposing team"
text <- Corpus(VectorSource(text))


text = tm_map(text, removePunctuation, preserve_intra_word_dashes = FALSE)
#docs <- tm_map(docs, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, tolower)
text <- tm_map(text, stripWhitespace) 
t<-newFreq(27,a)text[[1]])
text<-gsub(" ","",docs[[1]])
docs <- tm_map(docs, PlainTextDocument)
inspect(docs[1])
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs) 
inspect(dtm[1, 1:20])
TermDocumentMatrix

sourceCpp("StringFunctions.cpp")
acc<-accuracy("aasdf","asdfs")
a<-Freq(26,text)

sourceCpp("DecipheringMetropolis.cpp")
objdens(matrix(c(1,1,1,1),nrow=2),matrix(seq(1,4),nrow=2),2,100,4)

proposal(c(1,2,3,4,5),1)  

redCipher <-function( C, nSubs){
  n<-ncol(C)
  for (i in n:(nSubs+1)){
   # print i
    subs<-C[1,i]
    new<-C[2,i]
      for (j  in 1:i){
        if (C[2,j]==subs){
          C[2,j]<-new
        } 
    }
    C<-C[,-i]
     
  }
  C
}
a<-redCipher(matrix(c(1,2,3,4,2,3,4,1,2,3),nrow = 2),4)
b<-CiDecipher(redCipher(matrix(c(1,2,3,4,2,3,4,1,2,3),nrow = 2),4),"asdfasd")

chr <- function(n) { rawToChar(as.raw(n)) }#<br><br>chr(asc("a")) # 97<br>[1] "a"<br>
chr(61)

asc <- function(x) { strtoi(charToRaw(x),16L) }#<br><br>asc("a")<br>[1] 97<br>
asc('A')


#Referencia de pares de letras en textos en ingles
fe<-matrix(c(1, 11, 31, 48, 110, 25, 24, 114, 10, 2, 6, 40, 44, 40, 16, 23, 0, 50, 67, 59, 7, 5, 66, 1, 18, 1,
             20, 1, 0, 20, 23, 2, 3, 2, 5, 0, 1, 3, 7, 7, 12, 1, 0, 7, 11, 10, 5, 0, 1, 0, 7, 0,
             33, 0, 4, 9, 45, 3, 2,2,32, 0,1,2, 1,25, 13 ,0,0,10, 17, 11, 12, 0, 1, 2, 6, 0,
             52,0,  0, 13, 126,  2,  2,  1,  33,  0, 1,  36,  1,  146,  18, 0,  0, 20 , 7,7,  7, 0,  2,  0, 6,  0,
             0,47, 38, 57,48,20,28,302,23,2,29,64,68,66, 5, 30, 0, 133, 74, 75, 7, 65, 39, 1, 14, 3,
             12, 0, 0, 11, 30, 11, 3, 2, 17, 0, 1, 10, 2, 8, 80, 1, 0, 8, 11, 9, 2, 0, 1, 0, 7, 0,
             18, 0, 0, 7,  15,  1, 4,  1, 25, 0, 0, 1, 1, 92, 7, 0, 0, 10,  4,  3,  14, 0, 0,  0, 3,  0,
             5 ,0 ,38 ,25 ,33 ,8 ,35, 6, 6,    0, 2, 4, 3, 16, 11, 3, 0, 12, 50, 330, 2, 0, 44, 0, 10, 0,
             39, 6, 10, 50, 41, 23, 18, 97, 1, 3, 14, 47, 25, 33, 12, 12, 0, 50, 49, 76, 8, 11, 39, 2, 11, 1,
             1, 1, 0, 3, 3, 1, 1, 0, 1, 0, 0, 0, 0, 2, 1, 0, 0, 1, 2, 1, 0, 0, 0, 0, 1, 0,
             12, 0, 18, 1, 5, 0, 0, 0, 8, 0, 0, 3, 0, 8, 13 ,0 ,0 ,8, 6, 2, 1, 0, 0, 0, 1, 0,
             57, 17, 9, 11, 55, 8, 7, 2, 37, 0, 2, 56, 1, 9, 26, 15, 0, 10, 13, 17, 34, 0, 2, 0, 4, 0,
             26, 0, 0, 14, 47, 5 ,3, 3, 37, 0, 1, 4 ,5 ,7, 48, 1 ,0, 14, 12 ,11, 8, 0, 1 ,0 ,6 ,0,
             181, 0 , 0, 16, 111, 1, 4, 1, 179, 0, 9, 2, 2, 8, 106,  0,  0, 16,  10,  7,  36, 0, 12,  0, 3,  0,
             1, 19, 45, 41, 33, 40, 23, 49, 24, 3 ,4, 41, 29, 60, 36, 21, 0, 55, 57, 115, 1, 4 ,29, 0 ,36 ,0,
             20, 0, 0, 6, 28, 2, 1, 1, 6 ,0, 0, 3, 11, 4, 15, 10, 0 ,6 ,20, 4, 16 ,0, 0, 3, 4, 0,
             1, 0, 1,  0, 2,  0,  0,  0, 0,  0, 0, 0, 0, 1,   0,  0,  0, 0,  2, 0,  0,  0, 0, 0, 0 ,0,
             75,  11, 11, 14,  169,  16, 12, 8, 27,  0, 0, 2,  3,  3,  84,  18, 0, 14,  4,  28,  44,  0, 3, 0, 3,  0,
             95,2, 1,35, 115,  5,  9,  5,  86,  0, 5, 11,  10, 33,  28,  5,  0, 37,  43,  34,  35,  0, 4 ,0 ,19,  0,
             104 ,1,    15, 56,  83, 37, 16, 32, 93, 0, 4 ,15, 9, 106, 57,  11, 0, 42,  109,  56,  48, 0, 4, 3, 20, 0,
             9, 21, 7, 10, 6, 8, 7, 8, 1, 8, 1, 8, 8, 6, 115, 6, 9, 12, 20, 17, 0, 0, 1, 0, 1, 0,
             20, 0, 0, 2, 24, 0, 0, 0, 14, 0, 0, 3, 0, 2, 12, 0, 0, 4, 2, 1, 0, 0, 0, 0, 1, 0,
             13, 0, 0, 19, 50, 3, 5, 4, 7, 0, 2, 5, 4, 12, 46, 1, 0, 11, 24, 31, 2, 0, 2, 0, 12, 0, 
             1, 0, 0, 0, 9, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             26, 11, 1, 10, 26, 2, 1, 4, 0, 0, 2, 31, 18, 11, 5, 1, 0, 21, 4, 16, 1, 1, 1, 0, 2, 0,
             1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=26) 


