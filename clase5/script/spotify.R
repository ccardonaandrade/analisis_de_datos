library(tidyverse)


# Working directory
setwd(dir = "/Users/ccard/Dropbox/analitica_datos/2024-II/slides/lecture5/data")

# https://www.kaggle.com/code/lusfernandotorres/spotify-top-hits-2000-2019-eda/data
data <- read.csv("spotify_data.csv")

# Top 10 más populares
data %>% 
  select(artist, popularity, danceability) %>% 
  arrange(desc(popularity)) %>% 
  head(10)

# Cómo le va a Bad Bunny?
data %>% 
  select(artist, song, popularity, danceability) %>% 
  filter(artist=="Bad Bunny")



# Plot
ggplot(data, aes(x = danceability, fill = factor(year))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "Distribución de Danceability por Año",
       x = "Danceability",
       y = "Densidad") +
  scale_fill_discrete(name = "Año") +
  theme_minimal()


años_interes <- c(2000, 2005, 2010, 2015, 2019)
data_filtrada <- data %>% filter(year %in% años_interes)

ggplot() +  # Density for the selected years
  geom_density(data = data_filtrada, aes(x = danceability, color = factor(year), fill = factor(year)), alpha = 0.4) +
  labs(title = "Distribución de Danceability (2000-2019)",
       x = "Danceability",
       y = "Densidad",
       color = "Año",
       fill = "Año") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 26),        # Increase plot title size
    axis.title.x = element_text(size = 20),      # Increase x-axis title size
    axis.title.y = element_text(size = 20),      # Increase y-axis title size
    axis.text = element_text(size = 16),         # Increase axis numbers size
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.title = element_text(size = 18)       # Increase legend title size
  )



ggplot() +  # Density for each selected year
  geom_density(data = data_filtrada, aes(x = danceability, color = factor(year), fill = factor(year)), alpha = 0.4) +
  facet_wrap(~ year) +
  labs(title = "Distribución de Danceability (2000-2019)",
       x = "Danceability",
       y = "Densidad") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 26),        # Increase plot title size
    axis.title.x = element_text(size = 20),      # Increase x-axis title size
    axis.title.y = element_text(size = 20),      # Increase y-axis title size
    axis.text = element_text(size = 16),         # Increase axis numbers size
    strip.text = element_text(size = 18)         # Increase facet titles size
  )



# Calculamos la media poblacional
media_poblacional <- mean(data$danceability, na.rm = TRUE)
media_poblacional

# Tomamos una muestra de 100 canciones
muestra <- data %>% sample_n(100)

# Calculamos la media muestral
media_muestral <- mean(muestra$danceability, na.rm = TRUE)
media_muestral

# Calculamos el error estándar
error_std <- sd(muestra$danceability, na.rm = TRUE) / sqrt(nrow(muestra))
error_std


# Definimos el nivel de confianza (e.g., 95%)
confidence_level <- 0.95
z_score <- qnorm((1 + confidence_level) / 2)
z_score

# Calculamos los intervalos de confianza
ic_inf <- media_muestral - z_score * error_std
ic_sup <- media_muestral + z_score * error_std

# Definamoslo como intervalo
ci <- c(ic_inf, ic_sup)
ci


# Definimos el nivel de confianza (e.g., 95%)

#########################

## Otra manera es usando el comando t.test 
result <- t.test(muestra$danceability, conf.level = 0.95)
result


# Extraemos los intervalos
ic_2 <- result$conf.int
ic_2



# Calculate confidence intervals at different levels
result_95 <- t.test(muestra$danceability, conf.level = 0.95)
result_90 <- t.test(muestra$danceability, conf.level = 0.90)
result_99 <- t.test(muestra$danceability, conf.level = 0.99)

# Extract confidence intervals
ic_95 <- result_95$conf.int
ic_90 <- result_90$conf.int
ic_99 <- result_99$conf.int

# Combine the confidence intervals into a data frame
ci_df <- data.frame(
  Level = c("90%", "95%", "99%"),
  Lower = c(ic_90[1], ic_95[1], ic_99[1]),
  Upper = c(ic_90[2], ic_95[2], ic_99[2])
)


ggplot(ci_df, aes(x = Level, ymin = Lower, ymax = Upper)) +
  geom_linerange(size = 1.5, aes(color = Level)) +           # Plot confidence intervals
  geom_point(aes(y = Lower), size = 3) +                    # Add lower bound points
  geom_point(aes(y = Upper), size = 3) +                    # Add upper bound points
  geom_hline(aes(yintercept = media_poblacional, linetype = "Media Poblacional"), 
             color = "black", size = 1) +                   # Add dashed line for population mean
  scale_linetype_manual(name = "", values = "dashed") +     # Customize dashed line in legend
  labs(title = "Intervalos de Confianza para la Danceability promedio",
       x = "Nivel de Confianza",
       y = "Danceability",
       color = "Nivel de Confianza") +
  theme_minimal() +
  theme(legend.position = "right")


#######################
# Lets do 100 ci

# Set seed for reproducibility
set.seed(13)

# Number of samples
n_samples <- 100

# Population mean (assumed or known)
population_mean <- mean(data$danceability, na.rm = TRUE)

# Function to compute confidence interval
compute_ci <- function(sampled_data) {
  result <- t.test(sampled_data$danceability, conf.level = 0.95)
  ci <- result$conf.int
  return(ci)
}

# Generate 100 confidence intervals
ci_list <- replicate(n_samples, compute_ci(sample_n(data, 100)), simplify = FALSE)

# Create a data frame for plotting
ci_df <- do.call(rbind, lapply(1:n_samples, function(i) {
  data.frame(
    lower = ci_list[[i]][1],
    upper = ci_list[[i]][2],
    include_mean = ci_list[[i]][1] <= population_mean & ci_list[[i]][2] >= population_mean,
    sample = i
  )
}))

# Plot the confidence intervals

ggplot(ci_df, aes(x = sample, ymin = lower, ymax = upper, color = include_mean)) +
  geom_linerange(size = 1) +
  geom_hline(aes(yintercept = population_mean, linetype = "Media Poblacional"), 
             color = "navy", size = 1) +
  scale_color_manual(values = c("#FFBF00", "#365E32"), labels = c("Si", "No")) +
  scale_linetype_manual(name = "", values = c("Media Poblacional" = "dashed")) +
  labs(title = "100 Intervalos de Confianza para el Danceability promedio",
       x = "Muestra",
       y = "Intervalo de Confianza",
       color = "Incluye la Media Poblacional") +
  ylim(0.60, 0.75) +  # Set y-axis limits
  theme_minimal()


##############
# Lets do it for a 90% confidence interval

# Number of samples
n_samples <- 100

# Population mean (assumed or known)
population_mean <- mean(data$danceability, na.rm = TRUE)

# Function to compute confidence interval
compute_ci_90 <- function(sampled_data) {
  result <- t.test(sampled_data$danceability, conf.level = 0.90)
  ci <- result$conf.int
  return(ci)
}

# Generate 100 confidence intervals
ci_list <- replicate(n_samples, compute_ci_90(sample_n(data, 100)), simplify = FALSE)

# Create a data frame for plotting
ci_df <- do.call(rbind, lapply(1:n_samples, function(i) {
  data.frame(
    lower = ci_list[[i]][1],
    upper = ci_list[[i]][2],
    include_mean = ci_list[[i]][1] <= population_mean & ci_list[[i]][2] >= population_mean,
    sample = i
  )
}))

# Plot the confidence intervals
ggplot(ci_df, aes(x = sample, ymin = lower, ymax = upper, color = include_mean)) +
  geom_linerange(size = 1) +
  geom_hline(aes(yintercept = population_mean, linetype = "Media Poblacional"), 
             color = "navy", size = 1) +
  scale_color_manual(values = c("#FFBF00", "#365E32"), labels = c("Si", "No")) +
  scale_linetype_manual(name = "", values = c("Media Poblacional" = "dashed")) +
  labs(title = "100 Intervalos de Confianza para el Danceability promedio",
       x = "Muestra",
       y = "Intervalo de Confianza",
       color = "Incluye la Media Poblacional") +
  ylim(0.60, 0.75) +  # Set y-axis limits
  theme_minimal()




#########################

library(ggridges)

ggplot(filtered_data, aes(x = danceability, y = factor(year))) +
  geom_density_ridges2() +
  labs(y = "Year",
       x = "Danceability")


ggplot(filtered_data, aes(x = danceability, y = factor(year), fill = factor(year))) +
  geom_density_ridges() +
  labs(y = "Year",
       x = "Danceability") +
  scale_fill_discrete(name = "Year")


ggplot(filtered_data, aes(x = danceability, y = factor(year), fill = stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Danceability", option = "C")+
  labs(y = "Year")
