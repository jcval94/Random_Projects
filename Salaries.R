#https://www.youtube.com/watch?v=nx5yhXAQLxw&t=2224s
library(ggplot2)
library(tidyverse)

major_DB<-read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-16/recent-grads.csv")

major_DB %>% names()
major_DB %>% head()

#Graficaremos los salarios por carera

major_DB %>% filter(Sample_size>50) %>% group_by(Major_category) %>%
  mutate(median_sal=median(Median)) %>% 
  arrange(desc(median_sal)) %>% #head(15) %>%
  ggplot(aes(reorder(Major_category,median_sal),median_sal))+
  geom_col()+coord_flip()
  
#Plot con los quartiles y rangos
#Median P25th  P75th

Unemployment_rate<- major_DB %>% filter(Sample_size>50) %>%
  select(Major,Median,Unemployment_rate)


Unemployment_rate %>% ggplot(aes(Median,Unemployment_rate))+
  geom_point()

Unemployment_rate %>% arrange(desc(Unemployment_rate)) %>% head(15) %>%
  ggplot(aes(reorder(Major,Unemployment_rate),Unemployment_rate))+
  geom_col()+coord_flip()
  
