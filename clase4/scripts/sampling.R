library(tidyverse)


dado <- c(1,2,3,4,5,6)

muestra_de_5 <- sample(dado, 5, replace=TRUE)

muestra_de_5

mean(muestra_de_5)


sample(dado, 5, replace = TRUE) %>% mean()


sample(dado, 5, replace = TRUE) %>% mean()

# Repetir 10 veces
# Tirarlo 5 veces
# Tomar la media
sample_means <- replicate(10, sample(dado, 5, replace = TRUE) %>% mean())
sample_means


# Convert to a data frame for ggplot2
df <- data.frame(sample_means = sample_means)

# Plot the histogram
ggplot(df, aes(x = sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "#348feb", color = "black") +
  labs(title = "Distribución de media muestral", x = "Media muestral", y = "Frecuencia") +
  theme_minimal()


# 100 medias muestrales
sample_means <- replicate(100, sample(dado, 5, replace = TRUE) %>% mean())
sample_means

# Convert to a data frame for ggplot2
df <- data.frame(sample_means = sample_means)

# Plot the histogram
ggplot(df, aes(x = sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "#348feb", color = "black") +
  labs(title = "Distribución de media muestral", x = "Media muestral", y = "Frecuencia") +
  theme_minimal()



# 1000 medias muestrales
sample_means <- replicate(1000, sample(dado, 5, replace = TRUE) %>% mean())
sample_means

# Calculate the mean of sample_means
mean_sample_means <- mean(sample_means)

# Convert to a data frame for ggplot2
df <- data.frame(sample_means = sample_means)

# Plot the histogram
ggplot(df, aes(x = sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "#348feb", color = "black") +
  labs(title = "Distribución de media muestral", x = "Media muestral", y = "Frecuencia") +
  theme_minimal()

# Plot the histogram with the mean
ggplot(df, aes(x = sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "#348feb", color = "black") +
  geom_vline(aes(xintercept = mean_sample_means), color = "darkred", linetype = "dashed", size = 1) +
  labs(title = "Distribución de media muestral", x = "Media muestral", y = "Frecuencia") +
  theme_minimal()


# 10000 medias muestrales
sample_means <- replicate(10000, sample(dado, 5, replace = TRUE) %>% mean())
sample_means
mean_sample_means <- mean(sample_means)

# Convert to a data frame for ggplot2
df <- data.frame(sample_means = sample_means)

# Plot the histogram
ggplot(df, aes(x = sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "#348feb", color = "black") +
  labs(title = "Distribución de media muestral", x = "Media muestral", y = "Frecuencia") +
  theme_minimal()



# Qué pasa con la desviación estándar?
sample_sd <- replicate(1000, sample(dado, 5, replace = TRUE) %>% sd())
sample_sd

# Convert to a data frame for ggplot2
df <- data.frame(sample_sd = sample_sd)

# Plot the histogram
ggplot(df, aes(x = sample_sd)) +
  geom_histogram(binwidth = 0.25, fill = "#348feb", color = "black") +
  labs(title = "Distribución de la Desv. Est. muestral", x = "Desviación Estándar muestral", y = "Frecuencia") +
  theme_minimal()


#### normal

# Define the x-axis range and normal distribution
x <- seq(-4, 4, length = 1000)
y <- dnorm(x)

# Create a data frame
df <- data.frame(x = x, y = y)

# Plot the normal distribution with highlighted regions
ggplot(df, aes(x = x, y = y)) +
  geom_area(aes(x = ifelse(x >= -1.96 & x <= 1.96, x, NA)), fill = "yellow", alpha = 0.5) +
  geom_area(aes(x = ifelse(x < -1.96, x, NA)), fill = "navy", alpha = 0.5) +
  geom_area(aes(x = ifelse(x > 1.96, x, NA)), fill = "navy", alpha = 0.5) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(breaks = c(-1.96, 0, 1.96)) +
  labs(title = "Distribución Normal",
       x = "Z-score", y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "none")



## Normal with a different value

x <- seq(2.5, 4.5, length = 1000)
mean_val <- 3.5
sd_val <- (3.7 - 3.5) / 2  # Standard deviation so that 3.7 is 2 SDs from the mean
y <- dnorm(x, mean = mean_val, sd = sd_val)

# Create a data frame
df <- data.frame(x = x, y = y)

# Plot the normal distribution with highlighted regions
ggplot(df, aes(x = x, y = y)) +
  geom_area(aes(x = ifelse(x <= 3.7, x, NA)), fill = "white", alpha = 0.5) +
  geom_area(aes(x = ifelse(x > 3.7, x, NA)), fill = "darkred", alpha = 0.5) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(breaks = c(3.5, 3.7)) +
  labs(title = "Normal Distribution",
       x = "X", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14))  # Increase the size of x and y axis text


ggplot(df, aes(x = x, y = y)) +
  geom_area(aes(x = ifelse(x <= 3.7, x, NA)), fill = "white", alpha = 0.5) +
  geom_area(aes(x = ifelse(x > 3.7, x, NA)), fill = "darkred", alpha = 0.5) +
  geom_line(color = "black", size = 1) +
  scale_x_continuous(breaks = c(3.5, 3.7), limits = c(3.2, 3.8)) +
  labs(title = "Normal Distribution",
       x = "X", y = "Density") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14)  # Increase the size of x and y axis text
  )
  