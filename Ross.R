library(tidyverse)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")


View(bob_rolibrary(tidyverse)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")


View(bob_ross)


#Temporalidad/binarismo
### Primer filtro
dim(bob_ross)


#Formato
bob_ross_G<-gather(bob_ross,key = element,value = presente,-EPISODE,-TITLE)
bob_ross_G<-bob_ross_G %>%  filter(presente==1)

#Limpieza
bob_ross_G$TITLE<-gsub('"',"",bob_ross_G$TITLE)
bob_ross_G$element<-gsub('_'," ",bob_ross_G$element)
bob_ross_G<-bob_ross_G[,-4]

#Feature Engineering
bob_ross_G <- extract(bob_ross_G,"EPISODE",c("Temporada","Episodio"),c("S(.*)E(.*)"),convert = T,remove = F)
bob_ross_G <- arrange(bob_ross_G,Temporada, Episodio)


#Elementos  más comunes:
head(arrange(count(bob_ross_G,element,sort = T),desc(n)),15) %>%
    ggplot(aes(reorder(element, n),n)) + geom_col() + coord_flip()

#Pinturas más pobladas

head(arrange(count(bob_ross_G,TITLE,sort = T),desc(n)),15) %>%
  ggplot(aes(reorder(TITLE, n),n)) + geom_col() + coord_flip()


###############################################################
###############################################################
#Cómo sus pinturas han cabiado con el tiempo?
#número de elementos en el tiempo
library(reshape2)

bob_ross_t<-dcast(bob_ross_G,element~Temporada,fun.aggregate = length,margins = T)
bob_ross_t<-bob_ross_t[order(-bob_ross_t[[33]]),]
#Estacionalidad
TREE<-as.numeric(bob_ross_t[1,-c(1,33)])
#TSA::periodogram(APPLE)
plot.ts(TREE)

#Tendencia


#Distribución
library(FitUltD)
FDistUlt(TREE,plot = T)[[3]]



#Porcentaje de cada elemento por temporada
#Episodios por temporada
bob_ross_G<-bob_ross_G %>% group_by(Temporada) %>%
  mutate(numero_epis=n_distinct(EPISODE))

#Porcentaje de X elemento por temporada
MOUNTAINS<-bob_ross_G[bob_ross_G$element %in% "MOUNTAIN",] %>%
  group_by(Temporada) %>%
  mutate(porcentaje=n_distinct(EPISODE)/numero_epis)

MOUNTAINS[,c("Temporada","porcentaje")] %>%
  ggplot(aes(Temporada,porcentaje)) +
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

#Porcentaje de X por temporada

table(bob_ross_G[,c("Temporada","element")])


#Análisis de todos los elementos
bob_ross_TE<-dcast(bob_ross_G,Temporada~element,length,value.var="element",margins = F)
bob_ross_TE
# bob_ross_G %>% group_by(Temporada) %>% group_by(element) %>%
#   mutate(total_elementos=length(element))

elementos_temporada<-bob_ross_G %>% 
  count(Temporada, element, numero_epis) %>%
  mutate(porcentaje_elementos_por_episodios=n/numero_epis) %>%
  group_by(element) %>%
  mutate(total_elementos=sum(n)) %>% 
  ungroup()


elementos_temporada[elementos_temporada$total_elementos>100,] %>%
  ggplot(aes(Temporada,porcentaje_elementos_por_episodios,color=element))+
  geom_line()+
  facet_wrap(~element)


####Corr
library(widyr)
bob_ross_cor<-bob_ross_G %>% add_count(element) %>% filter(n>=4) %>%
  pairwise_cor(element,EPISODE,sort=T)


bob_ross_cor %>% filter(item1=="WAVES") %>%
  ggplot(aes(reorder(item2,correlation),correlation))+
  geom_col()+coord_flip()


bob_ross_cor %>% filter(item1=="SNOW") %>%
  ggplot(aes(reorder(item2,correlation),correlation))+
  geom_col()+coord_flip()

##OutLiers

bob_ross_cor

#Correlation clustering
library(ggraph)
library(igraph)

head(bob_ross_cor,100) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(color=-correlation)) +
  geom_node_point() +
  geom_node_text(aes(label=name,label.size=.1),
                 vjust=1,hjust=1) +
  theme_void()

tail(bob_ross_cor,100) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(color=correlation)) +
  geom_node_point() +
  geom_node_text(aes(label=name,label.size=.1),
                 vjust=1,hjust=1) +
  theme_gray()


###PCA
library(broom)

#Matriz elemento x capítulo
bob_ross_PC<-acast(bob_ross_G,TITLE~element)
#Se sustituyen los 0's por la media (negativa) y los 1 por 1 - media
#Sobre esa matriz se aplica "Singular Value Dec"
bob_ross_PC2<-svd(t(t(bob_ross_PC)-colMeans(bob_ross_PC)))

#Se toma la matriz de rotación "V*" -> trbajamos con los elementos
#Se transforma dicha matriz en un melt de Componentes Principales
bob_ross_PC3<-broom::tidy(bob_ross_PC2,matrix="v") %>%
  mutate(elemento = colnames(bob_ross_PC)[column])
#Se agregan los nombres

#Veamos el primer PC
bob_ross_PC3[bob_ross_PC3$PC==1,] %>%
  filter(abs(value)>.01) %>%
  ggplot(aes(reorder(elemento,value),value))+
  geom_col()+
  coord_flip()

library(tidytext)

bob_ross_PC3[bob_ross_PC3$PC<=4,] %>%
  filter(abs(value)>.05) %>%
  mutate(elemento=reorder_within(elemento,value,PC, sep = "")) %>%
  ggplot(aes(reorder(elemento,value),value,fill=factor(PC)))+
  geom_col()+
  coord_flip() +
  facet_wrap(~PC,scales = "free")


#Se toma la matriz de rotación "u*" -> trbajamos con los TITULOS (pinturas)
#Se transforma dicha matriz en un melt de Componentes Principales
bob_ross_PC4<-broom::tidy(bob_ross_PC2,matrix="u") %>%
  mutate(elemento = rownames(bob_ross_PC)[row])


bob_ross_PC4[bob_ross_PC4$PC==1,] %>%
  filter(abs(value)>.09) %>%
  ggplot(aes(reorder(elemento,value),value))+
  geom_col()+
  coord_flip()

library(tidytext)

bob_ross_PC4[bob_ross_PC4$PC<=4,] %>%
  # arrange(abs(value)) %>% 
  # head(56) %>%
  filter(abs(value)>.09) %>%
  mutate(elemento=reorder_within(elemento,value,PC, sep = "")) %>%
  ggplot(aes(reorder(elemento,value),value,fill=factor(PC)))+
  geom_col()+
  coord_flip() +
  facet_wrap(~PC,scales = "free")


#Variance explained
broom::tidy(bob_ross_PC2,matrix="d") %>%
  ggplot(aes(PC,percent))+
  geom_point()

ss)


#Temporalidad/binarismo
### Primer filtro
dim(bob_ross)


#Formato
bob_ross_G<-gather(bob_ross,key = element,value = presente,-EPISODE,-TITLE)
bob_ross_G<-bob_ross_G %>%  filter(presente==1)

#Limpieza
bob_ross_G$TITLE<-gsub('"',"",bob_ross_G$TITLE)
bob_ross_G$element<-gsub('_'," ",bob_ross_G$element)
bob_ross_G<-bob_ross_G[,-4]

#Feature Engineering
bob_ross_G <- extract(bob_ross_G,"EPISODE",c("Temporada","Episodio"),c("S(.*)E(.*)"),convert = T,remove = F)
bob_ross_G <- arrange(bob_ross_G,Temporada, Episodio)


#Elementos  más comunes:
head(arrange(count(bob_ross_G,element,sort = T),desc(n)),15) %>%
    ggplot(aes(reorder(element, n),n)) + geom_col() + coord_flip()

#Pinturas más pobladas

head(arrange(count(bob_ross_G,TITLE,sort = T),desc(n)),15) %>%
  ggplot(aes(reorder(TITLE, n),n)) + geom_col() + coord_flip()


###############################################################
###############################################################
#Cómo sus pinturas han cabiado con el tiempo?
#número de elementos en el tiempo
library(reshape2)

bob_ross_t<-dcast(bob_ross_G,element~Temporada,fun.aggregate = length,margins = T)
bob_ross_t<-bob_ross_t[order(-bob_ross_t[[33]]),]
#Estacionalidad
TREE<-as.numeric(bob_ross_t[1,-c(1,33)])
#TSA::periodogram(APPLE)
plot.ts(TREE)

#Tendencia


#Distribución
library(FitUltD)
FDistUlt(TREE,plot = T)[[3]]



#Porcentaje de cada elemento por temporada
#Episodios por temporada
bob_ross_G<-bob_ross_G %>% group_by(Temporada) %>%
  mutate(numero_epis=n_distinct(EPISODE))

#Porcentaje de X elemento por temporada
MOUNTAINS<-bob_ross_G[bob_ross_G$element %in% "MOUNTAIN",] %>%
  group_by(Temporada) %>%
  mutate(porcentaje=n_distinct(EPISODE)/numero_epis)

MOUNTAINS[,c("Temporada","porcentaje")] %>%
  ggplot(aes(Temporada,porcentaje)) +
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())

#Porcentaje de X por temporada

table(bob_ross_G[,c("Temporada","element")])


#Análisis de todos los elementos
bob_ross_TE<-dcast(bob_ross_G,Temporada~element,length,value.var="element",margins = F)
bob_ross_TE
# bob_ross_G %>% group_by(Temporada) %>% group_by(element) %>%
#   mutate(total_elementos=length(element))

elementos_temporada<-bob_ross_G %>% 
  count(Temporada, element, numero_epis) %>%
  mutate(porcentaje_elementos_por_episodios=n/numero_epis) %>%
  group_by(element) %>%
  mutate(total_elementos=sum(n)) %>% 
  ungroup()

elementos_temporada[elementos_temporada$total_elementos>100,] %>%
  ggplot(aes(Temporada,porcentaje_elementos_por_episodios,color=element))+
  geom_line()+
  facet_wrap(~element)




