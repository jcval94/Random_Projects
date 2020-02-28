#Text & Puzzles
#https://fivethirtyeight.com/features/can-you-solve-the-vexing-vexillology/



library(tidyverse)


palabras<-tibble(word=read_lines("https://norvig.com/ngrams/enable1.txt"))

palabras



letras<-c("a","p","x","m","e","l","g")
letra_central<-"g"
#Palabras que contengan g
palabras_g<-palabras[str_detect(palabras[[1]],letra_central),]


#longitud de la palabra >=4
palabras_g4<-palabras_g[str_length(palabras_g[[1]]) >=4,]

palabras2<-palabras_g4 %>% mutate(caracteres=str_length(word)) %>%
  mutate(puntos=ifelse(caracteres==4,1,caracteres)) %>%
  arrange(desc(puntos))

palabras2[["letras"]]<-strsplit(palabras2[["word"]],"")

#Quita estas letras a este conjunto de letras
setdiff(palabras2[["letras"]][[1000]],letras)

#Las palabras que se quedaron sin letras es porque
#les quité todas
palabras2[["Let_inval"]]<-map(palabras2[["letras"]],setdiff,letras)


palabras2[["Let_inval_len"]]<-lengths(map(palabras2[["letras"]],setdiff,letras))


palabras2<-palabras2[palabras2[["Let_inval_len"]]==0,]
# map(palabras2[palabras2[[1]]=="ag","Let_inval"],~nchar(.x[[1]]))
# map(palabras2[,"Let_inval"],~nchar(.x[[1]]))

#CREAR FUNCIÓN GENERADORA Y MAXIMIZAR PUNTOS
palabras2


puntos <- function(letras, letra_central) {
  if(!letra_central %in% letras | any(duplicated(letras))){
    return(warning("letra cantral dee estar en letras"))
    }
  palabras2<-palabras[str_detect(palabras[[1]],letra_central),] %>%
    filter(str_length(word) >=4) %>% 
    mutate(caracteres=str_length(word)) %>%
    mutate(puntos=ifelse(caracteres==4,1,caracteres)) %>%
    arrange(desc(puntos)) %>%
    mutate(letras=strsplit(word,"")) 
    # mutate(Let_inval=map(letras,setdiff,letras)) %>%
    # mutate(Let_inval_len=lengths(Let_inval)) %>%
    # filter(Let_inval_len==0)
  palabras2[["Let_inval"]]<-map(palabras2[["letras"]],setdiff,letras)
  palabras2[["Let_inval_len"]]<-lengths(palabras2[["Let_inval"]])
  palabras2<-palabras2[palabras2[["Let_inval_len"]]==0,]
  return(palabras2)
}


letras<-c("a","d","p","m","e","l","g")
letra_central<-"p"

HB<-puntos(letras,letra_central)

HB

