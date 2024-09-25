library(tidyverse)
library(readxl)
library(janitor)
setwd("C:/Users/ccard/Downloads")

data <- read_excel("KEL702-XLS-ENG.xls")


data <- read_excel("KEL702-XLS-ENG.xls", sheet = "Exhibit 1")


data <- data %>%
  clean_names()

data <- data %>% rename(us_gross = total_u_s_gross)
data <- data %>% rename(non_us_gross = total_non_u_s_gross)

summary(data$opening_gross)
summary(data$us_gross)
summary(data$non_us_gross)
summary(data$opening_theatres)


ggplot(data, aes(x=us_gross)) + geom_density()
ggplot(data, aes(x=us_gross)) + geom_histogram()
ggplot(data, aes(x=us_gross)) + geom_boxplot()



ggplot(data, aes(x=log(us_gross))) + geom_boxplot()


ggplot(data, aes(x=log(us_gross))) + geom_histogram()



xtabs(~genre, data=data)

xtabs(~mpaa, data=data)


### ROI

data <- data %>% 
  mutate(roi=(us_gross-budget)/budget)

mean(data$roi)

result <- t.test(data$roi, conf.level = 0.95)
result

ic<- result$conf.int
ic


## Comedy

data <- data %>%
  mutate(comedy = ifelse(genre == "Comedy", TRUE, FALSE))

# Perform a t-test comparing 'us_gross' for comedy and non-comedy movies
t_test_result <- t.test(us_gross ~ comedy, data = data)

# View the result
t_test_result


# Summarize the data to calculate mean and confidence intervals
summary_data <- data %>%
  group_by(comedy) %>%
  summarize(mean_us_gross = mean(us_gross, na.rm = TRUE),
            ci_lower = mean_us_gross - qt(0.975, df=n()-1) * sd(us_gross, na.rm = TRUE) / sqrt(n()),
            ci_upper = mean_us_gross + qt(0.975, df=n()-1) * sd(us_gross, na.rm = TRUE) / sqrt(n()))

# Plot mean 'us_gross' for comedy and non-comedy with confidence intervals
ggplot(summary_data, aes(x = factor(comedy, labels = c("Non-Comedy", "Comedy")), 
                         y = mean_us_gross)) +
  geom_point(size = 4) +  # Plot the mean as points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Add error bars for CIs
  labs(x = "Movie Type", y = "Mean US Gross", title = "Mean US Gross for Comedy vs Non-Comedy Movies") +
  theme_minimal()


ggplot(data, aes(x = factor(comedy, labels = c("Non-Comedy", "Comedy")), 
                 y = (us_gross/1000000))) +
  stat_summary(fun = mean, geom = "point", size = 4) +  # Plot the mean as points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +  # Add error bars for CIs
  labs(x = "Movie Type", y = "Mean US Gross", title = "Mean US Gross for Comedy vs Non-Comedy Movies") +
  theme_minimal()




# Perform a t-test comparing 'us_gross' for comedy and non-comedy movies
t_test_roi <- t.test(roi ~ comedy, data = data)

# View the result
t_test_roi



data <- data %>%
  mutate(r_rated = ifelse(mpaa == "R", TRUE, FALSE))
t_test_rated <- t.test(us_gross ~ r_rated, data = data)
t_test_rated
