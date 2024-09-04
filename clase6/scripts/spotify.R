library(tidyverse)
library(janitor)
library(patchwork)

# Working directory
setwd(dir = "/Users/ccard/Dropbox/analitica_datos/2024-II/slides/lecture5/data")

# https://www.kaggle.com/code/lusfernandotorres/spotify-top-hits-2000-2019-eda/data
spotify <- read.csv("spotify_data.csv")

# veamos las dimensiones
dim(spotify)


# Para ver los nombres de las variables
str(spotify)


# Calculemos algunas estadísticas

mean(spotify$popularity)
median(spotify$popularity)
sd(spotify$popularity)
var(spotify$popularity)
fivenum(spotify$popularity)
IQR(spotify$popularity)
min(spotify$popularity)
max(spotify$popularity)

# Funcion "aggregate" para descriptivas por grupo

aggregate(popularity ~ explicit , data=spotify, mean)

# Agreguemos otra categoría

aggregate(popularity ~ explicit + genre , data=spotify, mean)


# La tilde también funciona con las siguientes descriptivas:

aggregate(popularity ~ explicit , data=spotify, median)
aggregate(popularity ~ explicit , data=spotify, sd)
aggregate(popularity ~ explicit , data=spotify, var)
aggregate(popularity ~ explicit , data=spotify, min)
aggregate(popularity ~ explicit , data=spotify, max)
aggregate(popularity ~ explicit , data=spotify, IQR)


# Pasemos a visualizar los datos

ggplot(spotify, aes(x=popularity)) + geom_histogram()

# Qué tal un diagrama de caja

ggplot(spotify, aes(x=popularity)) + geom_boxplot()


# Usemos patchwork

histograma <- ggplot(spotify, aes(x=popularity)) + geom_histogram()
diag_caja <- ggplot(spotify, aes(x=popularity)) + geom_boxplot()
histograma+diag_caja

# Un diagrama de dispersión

ggplot(spotify, aes(x=danceability, y=popularity)) + geom_point()

spotify %>% 
  filter(popularity>0) %>%
  ggplot(aes(x=danceability, y=popularity)) + geom_point()


# Profundicemos en los histogramas


ggplot(spotify, aes(x=popularity)) + geom_histogram(binwidth=0.5)

ggplot(spotify, aes(x=popularity)) + geom_histogram(binwidth=0.1)

ggplot(spotify, aes(x=popularity)) + geom_histogram(binwidth=0.02)


# Y ahora en el diagrama de caja
ggplot(spotify, aes(x=popularity)) + geom_boxplot()
ggplot(spotify, aes(y=popularity)) + geom_boxplot()


# Diagrama de caja comparando entre categorías

ggplot(spotify, aes(x=popularity, y=explicit)) + geom_boxplot()
ggplot(spotify, aes(x=explicit, y=popularity)) + geom_boxplot()


# Incluyamos el genero musical
ggplot(subset(spotify, genre=="hip hop"), 
       aes(x=explicit, y=popularity)) + 
  geom_boxplot()

# Diagramas de dispersión

ggplot(spotify, aes(x=danceability, y=popularity, color=explicit)) + geom_point()


# Diagrama de dispersión por explicito en diferente color
ggplot(spotify, aes(x=danceability, y=log(1+popularity), color=explicit)) + 
  geom_point()

# Diagrama de dispersión por explicito en diferente forma o tamaño

ggplot(spotify, aes(x=danceability, y=popularity, shape=explicit)) + geom_point()
ggplot(spotify, aes(x=danceability, y=popularity, size=explicit)) + geom_point()

# combinando ambas
ggplot(spotify, aes(x=danceability,
                    y=popularity,
                    shape=explicit,
                    color=explicit)) + geom_point()

# Mostrar para cada categoría en diferentes facetas
ggplot(spotify, aes(x=danceability,y=popularity,color=explicit)) + 
  geom_point(size = 0.1)+
  facet_wrap(~explicit)


# Incluyamosle otra variable
spotify <- spotify %>%
  mutate(hip_hop_genre = ifelse(grepl("hip hop", genre, ignore.case = TRUE), TRUE, FALSE))
ggplot(spotify, aes(x=danceability,y=popularity,color=hip_hop_genre)) + 
  geom_point(size = 0.1)+
  facet_wrap(~explicit)


ggplot(spotify, aes(x=popularity)) + 
  geom_histogram(binwidth=0.5) +
  facet_wrap(~hip_hop_genre)


ggplot(spotify, aes(x=popularity)) + 
  geom_histogram(binwidth=0.5) +
  facet_wrap(~hip_hop_genre, nrow=5)


ggplot(spotify, aes(x=explicit,y=popularity)) + 
  geom_boxplot() +
  facet_wrap(~hip_hop_genre, nrow=2)
