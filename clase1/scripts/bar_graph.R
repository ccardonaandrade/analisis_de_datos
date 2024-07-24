#######################################################
# Este código es para crear el gráfico de barra en la diapositiva X de la Clase 1


# Recuerden installar los paquetes primero
# install.packages("gapminder")
# install.packages("dplyr")
# install.packages("tidyverse")

library(dplyr)
library(tidyverse)
library(gapminder)
library(countrycode)
library(ggimage)
library(rnaturalearth)
library(rnaturalearthdata)


# Damos un nombre a la base de datos "gapminder"
GAPMINDER<-gapminder

GAPMINDER<-filter(GAPMINDER, year == max(year))
GAPMINDER$iso2c<-countryname(GAPMINDER$country, destination = 'iso2c')


selected_countries <- GAPMINDER %>%
  filter(country %in% c('United States', 'China', 'Colombia', 'Brazil','Nigeria',  'Finland', 'Iran', 'India'))


selected_countries$iso2c<-countryname(selected_countries$country, destination = 'iso2c')


plot <- selected_countries %>% ggplot(aes(x= reorder(country, lifeExp),y=lifeExp))+ 
  geom_flag(y = -5, aes(image = iso2c)) +geom_bar(stat = "identity", color="black", fill = "maroon", width=0.6) + 
  labs(title = "Life Expectancy for Several Countries ",
       subtitle = "Source: GAPMINDER, 2007 ",
       x = "Country",
       y = "Life Expectancy")+ coord_flip() +
  expand_limits(y = -5) 

plot+ theme_minimal()



# Create the box plot
box_plot <- ggplot(GAPMINDER, aes(x = continent, y = lifeExp)) +
  geom_boxplot(fill = "lightblue", color = "black") +
 geom_jitter(width = 0.2, size = 2, color = "maroon")+
  labs(
    title = "Life Expectancy by Continent",
    subtitle = "Source: GAPMINDER, 2007",
    x = "Continent",
    y = "Life Expectancy"
  )
box_plot+ theme_minimal()



world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



# Rename the variable using rename()
GAPMINDER <- GAPMINDER %>%
  rename(iso_a2 = iso2c)
JOIN<-left_join(world,GAPMINDER, by="iso_a2")
ggplot(data = JOIN) +
  geom_sf(aes(fill = lifeExp)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  labs(fill = "Life Expectancy",title = "Life Expectancy by Continent",
       subtitle = "Source: GAPMINDER, 2007") + theme_bw() +  
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_text(size=14),
        legend.key = element_rect( fill = "white"),
        legend.position = c(0.15, 0.45),
        legend.key.size = unit(0.8, 'cm'),
        legend.text=element_text(size=14),
        legend.spacing.y = unit(0.1, "cm")) 
