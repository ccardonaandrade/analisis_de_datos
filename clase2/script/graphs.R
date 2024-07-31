

# Primer cargamos los paquetes
library(tidyverse)
library(ggplot2movies)
movies <- movies
top1000 <- movies %>%
  arrange(desc(budget)) %>%
  slice_head(n = 1000)
top1000 <- top1000 %>%
  mutate(mpaa = ifelse(mpaa == "", "NA", mpaa))


ggplot(data = top1000, mapping = aes(x = budget, y = rating)) + 
  geom_point()

ggplot(data = top1000, mapping = aes(x = budget/1000000, y = rating)) + 
  geom_point()

ggplot(data = top1000, aes(x = budget/1000000, y = rating, col = mpaa)) + 
  geom_point(alpha = 0.3)

ggplot(data = top1000, aes(x = budget/1000000, y = rating)) +  ## Applicable to all geoms
  geom_point(aes(col = mpaa), alpha = 0.3) ## Applicable to this geom only

#Error
ggplot(data = top1000, aes(x = budget/1000000, y = rating)) +  
  geom_point(aes(col = "black"), alpha = 0.3) 

#Name
p = ggplot(data = top1000, aes(x = budget/1000000, y = rating))
p

# More layers
p + 
  geom_point(alpha = 0.3)  +
  geom_smooth(method = "loess")

# Compare
p + 
  geom_point(aes(col = mpaa), alpha = 0.3)  +
  geom_smooth(method = "loess")


ggplot(data = top1000, aes(x = budget/1000000, y = rating, col = mpaa)) + 
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")

#Error 
p + geom_density()

ggplot(data = top1000) + ## i.e. No "global" aesthetic mappings"
  geom_density(aes(x = rating, fill = mpaa), alpha=0.3)


# Best plot
p2 <- ggplot(data = top1000, aes(x = budget, y = rating)) +
geom_point(aes(col = mpaa), alpha = 0.3) +
scale_color_brewer(name = "Mpaa", palette = "Set1") + ## Different colour scale
scale_x_log10(labels = scales::dollar)  + ## Switch to logarithmic scale on x-axis. Use dollar units.
labs(x = "Budget", y = "Rating in IMBD") + ## Better axis titles
theme_minimal() ## Try a minimal (b&w) plot theme


# With different theme
p2 + theme_economist()



###############
# Tablas

summary_table <- top1000 %>%
  count(mpaa) %>%  # Count the number of observations for each category
  mutate(percentage = n / sum(n) * 100) %>%  # Calculate the percentage
  arrange(desc(percentage))  # Arrange the table in descending order of percentage

# Print the summary table
print(summary_table)


top1000 <- top1000 %>%
  mutate(
    rating_status = ifelse(rating > mean(rating, na.rm = TRUE), "Above average", "Below average")
  )

cross_tab <- table(top1000$mpaa, top1000$rating_status)
# Add row and column totals
cross_tab_with_totals <- addmargins(cross_tab)

# Print the cross-tabulation table with totals
print(cross_tab_with_totals)


# Percentages
cross_tab <- table(top1000$mpaa, top1000$rating_status)

# Calculate the total number of observations
total_sum <- sum(cross_tab)

# Calculate percentages
percentages <- (cross_tab / total_sum) * 100

# Add row and column totals
row_totals <- rowSums(percentages)
col_totals <- colSums(percentages)
total_row <- c(row_totals, sum(percentages))

# Combine into a single table
percentages_with_totals <- rbind(
  cbind(percentages, Total = row_totals),
  Total = c(col_totals, sum(percentages))
)

# Print the table with percentages and totals
print(percentages_with_totals)


#####
# Graph Bar

# Calculate the percentages
percentages <- top1000 %>%
  count(mpaa) %>%
  mutate(Percentage = (n / sum(n)) * 100) %>%
  arrange(Percentage) %>%  # Arrange from highest to lowest
  mutate(mpaa = factor(mpaa, levels = mpaa))  # Reorder factor levels

ggplot(percentages, aes(x = mpaa, y = Percentage, fill = mpaa)) +
  geom_bar(stat = "identity") +
  labs(x = "MPAA Rating", y = "Percentage (%)", title = "Percentage of Observations by MPAA Rating") +
  theme_minimal() +
  coord_flip()  # Optional: Flip coordinates for better readability


# Pie Chart
percentages <- percentages %>%
  mutate(Fraction = Percentage / 100,
         ymin = cumsum(Fraction) - Fraction,
         ymax = cumsum(Fraction))

# Plot the pie chart
bp<- ggplot(percentages, aes(x="", y=Percentage, fill=mpaa))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie + scale_fill_grey()  +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Percentage/3 + c(0, cumsum(Percentage)[-length(Percentage)]), 
                label = percent(Percentage/100)), size=4)


## Bivariate bar graph

bivar_percentages <- top1000 %>%
  group_by(mpaa, rating_status) %>%
  summarise(count = n()) %>%
  mutate(Percentage = (count / 1000) * 100) %>%
  ungroup()

ggplot(bivar_percentages, aes(x = mpaa, y = Percentage, fill = rating_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  labs(x = "MPAA", y = "Percentage", fill = "Rating Position", title = "Percentage of Observations by MPAA Rating and Rating Status") + 
  theme_minimal() +
  theme(legend.position = "right")  # Optional: Move legend to the top



######
# Frecuencias

top1000 <- top1000 %>%
  mutate(rating_category = cut(rating,
                               breaks = c(0, 2, 4, 6, 8, 10), ## Specifies the boundaries for the intervals. The intervals are (0, 2], (2, 4], (4, 6], (6, 8], and (8, 10].
                               labels = c("1-2", "3-4", "5-6", "7-8", "9-10"),
                               right = TRUE)) ## Indicates that the intervals are right-closed (i.e., the endpoint is included in the interval).


freq_table <- top1000 %>%
  group_by(rating_category) %>%
  summarise(
    frequency = n(),
    relative_frequency = frequency / nrow(top1000),
    percentage = relative_frequency * 100
  ) %>%
  arrange(rating_category)  # Optional: Arrange by rating_category

# Rename columns for clarity
freq_table <- freq_table %>%
  rename(
    category = rating_category,
    "relative frequency" = relative_frequency,
    "%" = percentage
  )

# Print the summary table
print(freq_table)

## Ahora es acumulada
summary_table <- top1000 %>%
  group_by(rating_category) %>%
  summarise(
    frequency = n(),
    relative_frequency = frequency / nrow(top1000),
    percentage = relative_frequency * 100
  ) %>%
  arrange(rating_category) %>%  # Arrange by rating_category
  mutate(cumulative_relative_frequency = cumsum(relative_frequency))

# Rename columns for clarity
summary_table <- summary_table %>%
  rename(
    category = rating_category,
    "relative frequency" = relative_frequency,
    "%" = percentage,
    "cumulative relative frequency" = cumulative_relative_frequency
  )

# Print the summary table
print(summary_table)


# Histogram

ggplot(top1000, aes(x=rating)) + geom_histogram(color="darkblue", fill="lightblue") +
  labs(x = "Rating", y = "Count") +
  theme_minimal()


ggplot(top1000, aes(x = rating)) +
  geom_histogram(color = "darkblue", fill = "lightblue", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(x = "Rating", y = "Count") +
  theme_minimal()
