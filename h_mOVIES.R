#https://www.youtube.com/watch?v=yFRSTlk3kRQ

#Hrrm movies pred
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

library(tidyverse)
horror_movies

#Mejores pelis

horror_movies %>% arrange(desc(review_rating))
