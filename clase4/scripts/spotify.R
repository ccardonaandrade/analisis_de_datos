library(tidyverse)


# Working directory
setwd(dir = "/Users/ccard/Dropbox/analitica_datos/2024-II/slides/lecture4/data")

# https://www.kaggle.com/code/lusfernandotorres/spotify-top-hits-2000-2019-eda/data
data <- read.csv("spotify_data.csv")

# Plot
ggplot(data, aes(x = danceability, fill = factor(year))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ year) +
  labs(title = "Distribution of Danceability by Year",
       x = "Danceability",
       y = "Density") +
  theme_minimal()



selected_years <- c(2000, 2005, 2010, 2015, 2019)
filtered_data <- data %>% filter(year %in% selected_years)

ggplot() +
  # Density of the entire dataset in gray
  geom_density(data = data, aes(x = danceability), color = "gray", fill = "gray", alpha = 0.2) +
  # Density for the selected years
  geom_density(data = filtered_data, aes(x = danceability, color = factor(year), fill = factor(year)), alpha = 0.4) +
  labs(title = "Distribution of Danceability for Selected Years with Overall Density",
       x = "Danceability",
       y = "Density",
       color = "Year",
       fill = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom")



ggplot() +  # Density for each selected year
  geom_density(data = filtered_data, aes(x = danceability, color = factor(year), fill = factor(year)), alpha = 0.4) +
  facet_wrap(~ year) +
  labs(title = "Danceability Distribution for Selected Years with Overall Density",
       x = "Danceability",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")




mean(data$danceability, na.rm = TRUE)



sampled_data <- data %>% sample_n(100)
mean(sampled_data$danceability)



# Calculate the sample mean and standard error
mean_danceability <- mean(sampled_data$danceability, na.rm = TRUE)
std_error <- sd(sampled_data$danceability, na.rm = TRUE) / sqrt(nrow(sampled_data))

# Define the confidence level (e.g., 95%)
confidence_level <- 0.95
z_score <- qnorm((1 + confidence_level) / 2)

# Compute the confidence intervals
ci_lower <- mean_danceability - z_score * std_error
ci_upper <- mean_danceability + z_score * std_error

# Display the confidence intervals
ci <- c(ci_lower, ci_upper)
ci

#########################
## Another way is 
result <- t.test(sampled_data$danceability, conf.level = 0.95)

# Extract the confidence interval
ci_2 <- result$conf.int
ci_2



#######################
# Lets do 100 ci

# Set seed for reproducibility
set.seed(123)

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
  geom_hline(yintercept = population_mean, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("darkred", "navy"), labels = c("Excludes Mean", "Includes Mean")) +
  labs(title = "100 Confidence Intervals for Danceability Mean",
       x = "Sample",
       y = "Confidence Interval",
       color = "Includes Population Mean") +
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
  geom_hline(yintercept = population_mean, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("darkred", "navy"), labels = c("Excludes Mean", "Includes Mean")) +
  labs(title = "100 Confidence Intervals for Danceability Mean",
       x = "Sample",
       y = "Confidence Interval",
       color = "Includes Population Mean") +
  theme_minimal()
