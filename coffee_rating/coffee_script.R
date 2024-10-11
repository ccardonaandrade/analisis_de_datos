library(tidyverse)
library(readxl)
library(broom)
library(corrplot)


# Estableciendo el directorio   
setwd("C:/Users/ccard/OneDrive/Documentos/GitHub/analisis_de_datos/coffee_rating")


###################################
#       PARTE Descriptiva
###################################

# Punto 1
# Cargando los datos 
coffee_data <- read.csv("coffee_ratings.csv")



# Punto 2
coffee_data <- coffee_data %>%
  filter(!total_cup_points==0)


# Punto 3
ggplot(coffee_data, aes(x=total_cup_points))+
  geom_density()+
  labs(title = "Density of Total Cup Points",
       x = "Total Cup Points") + 
  theme_minimal()

ggplot(coffee_data, aes(x=total_cup_points))+
  geom_boxplot()+
  labs(title = "Boxplot of Total Cup Points",
       x = "Total Cup Points") + 
  theme_minimal()


summary(coffee_data$total_cup_points)


# Punto 4
ggplot(coffee_data, aes(x=aroma))+
  geom_density()+
  labs(title = "Density of Aroma",
       x = "Aroma") + 
  theme_minimal()

ggplot(coffee_data, aes(x=aroma))+
  geom_boxplot()+
  labs(title = "Boxplot of Aroma",
       x = "Aroma") + 
  theme_minimal()


summary(coffee_data$aroma)

# Punto 5

ggplot(coffee_data, aes(x=aroma, y=total_cup_points))+
  geom_point() +
  labs(title = "Scatterplot",
       x = "Aroma",
       y = "Total Cup Points") + 
  theme_minimal()

cor(coffee_data$total_cup_points, coffee_data$aroma)


# Punto 6

subset_coffee <- coffee_data %>%
  select(aroma, flavor, aftertaste, acidity, body)

corrplot(cor(subset_coffee ),
         method = "number",
         type = "upper")


###################################
#       PARTE INFERENCIAL
###################################

# Punto 7 y 8
coffee_model <- lm(total_cup_points ~ aroma, coffee_data)
tidy(coffee_model,conf.int = TRUE)

# Punto 9

ggplot(coffee_data, aes(x=total_cup_points))+
  geom_density()+ geom_vline(xintercept=38,
                             linetype = "dashed",
                             color="darkred")+
  labs(title = "Density of Total Cup Points",
       x = "Total Cup Points") + 
  theme_minimal()

# Punto 10

# Linealidad y varianza
augment(coffee_model) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Valores predichos", y = "Residuos")+
  theme_minimal()

# Normalidad
augment(coffee_model) %>%
  ggplot(aes(x = .resid)) +
  geom_histogram() +
  labs(title = "Distribución de los residuos",
       x = "Residuos") +
  theme_minimal()

# Punto 11
tidy(coffee_model,conf.int = TRUE)

# Punto 12
glance(coffee_model)

# Punto 13
coffee_data <- coffee_data %>%
  mutate(colombia = ifelse(country_of_origin == "Colombia", 1, 0))
mean(coffee_data$colombia, na.rm = TRUE)


# Punto 14
coffee_model_colombia <- lm(total_cup_points ~ aroma + colombia, coffee_data)
tidy(coffee_model_colombia,conf.int = TRUE)

# Punto 15
glance(coffee_model_colombia)

# Punto 16
model_flavor <- lm(total_cup_points ~ flavor, coffee_data)
glance(model_flavor)

# Punto 17
model_aroma_flavor <- lm(total_cup_points ~ aroma + flavor, coffee_data)
tidy(model_aroma_flavor,conf.int = TRUE)


# Punto 18
coffee_data <- coffee_data %>% 
  mutate(log_cup_points=log(total_cup_points),
         log_body=log(body))
model_body <- lm(log_cup_points ~ log_body, coffee_data)
tidy(model_body,conf.int = TRUE )
