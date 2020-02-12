#Text & Puzzles
#https://fivethirtyeight.com/features/can-you-solve-the-vexing-vexillology/



library(tidyverse)


palabras<-tibble(word=read_lines("https://norvig.com/ngrams/enable1.txt"))

palabras


letras<-c("a","p","x","m","e","l")
#Palabras que contengan g
palabras_g<-palabras[str_detect(palabras[[1]],"g"),]


#longitud de la palabra >=4
palabras_g4<-palabras_g[str_length(palabras_g[[1]]) >=4,]


