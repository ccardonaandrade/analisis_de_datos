library(tidyverse)
library(readxl)
library(janitor)
library(broom)
library(corrplot)



hollywood <- read_excel("C:/Users/ccard/Downloads/KEL702-XLS-ENG.xls", sheet = "Exhibit 1")
hollywood <- hollywood %>%
  clean_names()
hollywood <- hollywood %>% rename(us_gross = total_u_s_gross)
hollywood <- hollywood %>% rename(non_us_gross = total_non_u_s_gross)

glimpse(hollywood)


hollywood <- hollywood %>%
  mutate(comedy = ifelse(genre == "Comedy", 1, 0))
hollywood <- hollywood %>%
  mutate(r_rated = ifelse(mpaa == "R", 1, 0))



###########
#   Part 5
###########

hollywood_model <- lm(us_gross ~ budget + r_rated + sequel, data=hollywood)
tidy(hollywood_model, conf.int = TRUE)

hollywood_model <- lm(us_gross ~ budget  + sequel, data=hollywood)
tidy(hollywood_model, conf.int = TRUE)


###########
#   Part 6
###########

opening_model <- lm(opening_gross ~ budget + r_rated + sequel+ summer + holiday + christmas + opening_theatres , data=hollywood)
tidy(opening_model, conf.int = TRUE)


opening_model <- lm(opening_gross ~  budget + sequel+ summer + opening_theatres , data=hollywood)
tidy(opening_model, conf.int = TRUE)



###########
#   Part 7
###########

model_part7 <- lm(us_gross ~ opening_gross, data=hollywood)
tidy(model_part7, conf.int = TRUE)
glance(model_part7)


###########
#   Part 8
###########

model_part8 <- lm(us_gross ~ opening_gross + budget + r_rated + sequel+ summer +
                    holiday + christmas + opening_theatres + critics_opinion +
                    known_story + origin_united_states , data=hollywood)
tidy(model_part8, conf.int = TRUE)



model_part8 <- lm(us_gross ~ opening_gross + budget + r_rated  + critics_opinion , data=hollywood)
tidy(model_part8, conf.int = TRUE)


# Filter the movie "Flags of Our Fathers"
movie_data <- hollywood %>% 
  filter(movie == "Flags of Our Fathers") %>% 
  select(opening_gross, budget, r_rated, critics_opinion)

# Calculate the point estimate and 95% confidence interval
pred <- predict(model_part8, newdata = movie_data, interval = "confidence", level = 0.95)

pred


# Predicción a mano
-30038070+(2.82*10245190)+(0.259*90000000)-11141788+(590794*79)



###########
#   Part 9
###########
              
model_part9<- lm(us_gross ~ opening_gross + budget + r_rated 
                 + comedy * critics_opinion , data=hollywood)
tidy(model_part9, conf.int = TRUE)
