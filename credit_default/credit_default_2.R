library(tidyverse)
library(broom)
library(janitor)

credit_demo <- read_csv("C:/Users/ccard/Dropbox/analitica_datos/2024-II/ejercicios/credit_default/credit_demographics.csv")
credit_pay <- read_csv("C:/Users/ccard/Dropbox/analitica_datos/2024-II/ejercicios/credit_default/credit_payments.csv")


credit_pay <- credit_pay %>%
  pivot_wider(
    names_from = month,                               # Use month as the new column names
    values_from = c(repayment_status, payment_amount),  # Specify which columns to widen
    names_sep = "_"                                   # Separator for new column names
  )


credit_pay <- credit_pay %>%
  select(id, repayment_status_september, payment_amount_september)

credit_pay <- credit_pay %>%
  rename(ID = id)


final_data <- left_join(credit_demo, credit_pay)

rm(credit_demo, credit_pay)


final_data <- final_data %>%
  clean_names()


final_data <- final_data %>%
  mutate(
    default_string = case_when(
      default == 1 ~ "Default",
      default == 0 ~ "No Default",
      TRUE ~ NA_character_  # Assign NA for any unmatched values
    ),
    sex_string = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_  # Assign NA for any unmatched values
    ),
    education_string = case_when(
      education == 1 ~ "Graduate School",
      education == 2 ~ "University",
      education == 3 ~ "High School",
      education == 4 ~ "Others",
      education == 5 ~ "Unknown",
      education == 6 ~ "Unknown",
      TRUE ~ NA_character_  # Assign NA for any unmatched values
    ),
    marriage_string = case_when(
      marriage == 1 ~ "Married",
      marriage == 2 ~ "Single",
      marriage == 3 ~ "Others",
      TRUE ~ NA_character_  # Assign NA for any unmatched values
    )
  )


ggplot(final_data, aes(x = default_string)) + 
  geom_bar() + 
  labs(x = "",
       y = "Frequency") +
  scale_y_continuous(limits = c(0, 30000)) + 
  theme_minimal()


ggplot(final_data, aes(fill=sex_string, x=default_string)) + 
  geom_bar() +
  theme_minimal()



ggplot(final_data, aes(fill=sex_string, x=default_string)) + 
  geom_bar(position="fill") +
  labs(fill='Sex',
       x = "",
       y = "Proportion") +
  theme_minimal()


ggplot(final_data, aes(fill=marriage_string, x=default_string)) + 
  geom_bar(position="fill") +
  labs(fill='Sex',
       x = "",
       y = "Proportion") +
  theme_minimal()

model_sex <- lm(default ~ factor(marriage_string), final_data)
tidy(model_sex)

ggplot(final_data, aes(x=limit_bal)) + geom_density()
ggplot(final_data, aes(x=log(limit_bal))) + geom_density()
ggplot(final_data, aes(x=log(limit_bal))) + geom_histogram()

  
  
ggplot(final_data, aes(x=age, y=limit_bal)) + geom_point()


model_pay <- lm(log(1+payment_amount_september) ~ log(1+limit_bal), final_data)
tidy(model_pay)



final_model <- lm(default ~ factor(sex_string) + factor(education_string) +
                    age+I(age^2) , final_data)
tidy(final_model)


ggplot(final_data) +
  geom_smooth(aes(x = age, y = default), method = "loess", color = "darkred") +
  geom_smooth(aes(x = age, y = default), method = "lm", color = "navy") +
  theme_minimal()


ggplot(final_data, aes(y=log(payment_amount_september), x=log(limit_bal))) + 
  geom_point() +
  scale_y_continuous(limits = c(-1, 15)) +
  scale_x_continuous(limits = c(0, 15)) +
  theme_minimal() 


ggplot(final_data, aes(y=log(payment_amount_september), x=age)) + 
  geom_point() +
  theme_minimal() 
