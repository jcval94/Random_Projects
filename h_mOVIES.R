#https://www.youtube.com/watch?v=yFRSTlk3kRQ

#Hrrm movies pred
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

library(tidyverse)
horror_movies

#Mejores pelis

HM_1<-horror_movies %>% arrange(desc(review_rating)) %>%
  extract(title,"anio","\\((\\d\\d\\d\\d)\\)",remove=F,convert = T)

HM_1$budget<-parse_number(HM_1$budget)

HM_1 %>% ggplot(aes(anio))+geom_histogram()

#La mayoría de las pelis están depués de 2010

HM_1 %>% count(genres,sort = T)

HM_1 %>% count(language,sort = T)

#Retiramos el formato
HM_1 %>% count(budget,sort = T) %>%
  ggplot(aes(budget))+geom_histogram()+
  scale_x_log10(labels=scales::dollar)

#Peliculas con más presupuesto tienen mejor rating?


HM_1 %>% ggplot(aes(budget,review_rating)) +
  geom_point() + scale_x_log10(labels=scales::dollar)+
  geom_smooth(method = "glm")
#No parece haber una correlaci{on}

HM_1 %>% select(budget,review_rating) %>% 
  filter(!is.na(budget) & !is.na(review_rating)) %>%
  cor()
#En efecto, es muy baja la correlaci{on}

HM_1 %>% mutate(movie_rating=fct_lump(movie_rating,5)) %>%
  count(movie_rating,sort = T) %>% 
  ggplot(aes(reorder(movie_rating,n),n)) +
  geom_col()+coord_flip()

#Veremos la variaci{on de los reviews respecto a la clasificacion
#de la pelicula
HM_1 %>% mutate(movie_rating=fct_lump(movie_rating,5)) %>%
  ggplot(aes(reorder(movie_rating,review_rating),review_rating)) +
  geom_boxplot()+coord_flip()

#Analizaremos la varianza

#Primero debemos revisar si la dist. es normal
library(FitUltD)
Fit<-FDist(na.omit(HM_1$review_rating),plot = T)
Fit[[4]]
#Efectivamente, p.valores >0.05, ahora podemos utilizar anova()

#Retiramos na's
HM_1 %>% mutate(movie_rating=fct_lump(movie_rating,5)) %>%
  filter(!is.na(movie_rating)) %>%
  lm(review_rating~movie_rating,data=.) %>%
  anova()
  
#Dado que Pr(>F) (p.valor) es muy cercano a 0 entonces rechazamos que
#tienen la misma varianza, es decir, el tipo de pel{icula} afecta la calificación


#Analisis de columnas con "|" como separador 
#(aquellas con más d euna clasificación)
HM_1 %>% filter(!is.na(genres)) %>% 
  separate_rows(genres,sep = "\\| ") %>%
  mutate(genres=fct_lump(genres,5)) %>%
  ggplot(aes(reorder(genres,review_rating),review_rating)) +
  geom_boxplot()+coord_flip()


HM_1 %>% filter(!is.na(genres)) %>% 
  separate_rows(genres,sep = "\\| ") %>%
  mutate(genres=fct_lump(genres,5)) %>% 
  lm(review_rating~genres,data = .) %>%
  anova()

#mismo resultado para el g{enero}

#Ahora separaremos la columna plot

