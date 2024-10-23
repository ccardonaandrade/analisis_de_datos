library(tidyverse)
library(broom)

credit <- read_csv("C:/Users/ccard/Dropbox/analitica_datos/2024-II/ejercicios/credit_default/UCI_Credit_Card.csv")
credit_demographic <- credit %>%
  select(ID,LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE, default.payment.next.month)

credit_demographic <- credit_demographic %>%
  rename(DEFAULT = default.payment.next.month)
  
  
credit_default <- credit %>%
  select(ID, starts_with("PAY"))

credit_default <- credit_default %>%
  rename_with(~ gsub("^PAY_AMT", "AMT", .), starts_with("PAY_AMT"))


credit_default <- credit_default %>%
  rename(PAY_1=PAY_0)

df_pay_long <- credit_default %>%
  pivot_longer(
    cols = starts_with("PAY_"),   # Select columns starting with PAY_
    names_to = c("variable", "month"),
    names_pattern = "(PAY)_(\\d+)",    # Extract "PAY" and month number
    values_to = "repayment_status"
  )

df_pay_long <- df_pay_long %>%
  select(ID,month,repayment_status)

# Pivot AMT columns (payment amount)
df_amt_long <- credit_default %>%
  pivot_longer(
    cols = starts_with("AMT"),    # Select columns starting with AMT
    names_to = c("variable", "month"),
    names_pattern = "(AMT)(\\d+)",     # Extract "AMT" and month number
    values_to = "payment_amount"
  )

df_amt_long <- df_amt_long %>%
  select(ID,month,payment_amount)

df_long <- df_pay_long %>%
  left_join(df_amt_long, by = c("ID", "month"))
df_long <- df_long %>%
  mutate(month = recode(month,
                        `1` = "september",
                        `2` = "august",
                        `3` = "july",
                        `4` = "june",
                        `5` = "may",
                        `6` = "april"))


rm(df_amt_long, df_pay_long)
df_long <- df_long %>%
  rename(id = ID)


df_long %>% filter(month=="september") %>%
  select(repayment_status) %>%
  distinct() %>% pull()


df_wide <- df_long %>%
  pivot_wider(
    names_from = month,                               # Use month as the new column names
    values_from = c(repayment_status, payment_amount),  # Specify which columns to widen
    names_sep = "_"                                   # Separator for new column names
  )


write.csv(credit_demographic, "C:/Users/ccard/Dropbox/analitica_datos/2024-II/ejercicios/credit_default/credit_demographics.csv")
write.csv(df_long, "C:/Users/ccard/Dropbox/analitica_datos/2024-II/ejercicios/credit_default/credit_payments.csv")
