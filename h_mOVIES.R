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



