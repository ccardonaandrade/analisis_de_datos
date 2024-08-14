library(tidyverse)


# Weakness of averages
# Graph


# Define the values of 'a'
a_values <- c(5, 10, 15, 20)

# Create a data frame for plotting
df <- data.frame(x = rep(c(2, 3, 4, 5, 6, 7), times = length(a_values)),
                 y = rep(0, length(a_values) * 6),
                 a = rep(a_values, each = 6))

# Calculate means for each 'a'
means <- sapply(a_values, function(a) mean(c(2, 3, 4, 5, 6, 7, a)))

# Create a data frame for means
df_means <- data.frame(a = a_values, avg = means)

# Create the plot
ggplot(df, aes(x = x, y = y)) +
  geom_point() + # Add points
  geom_hline(yintercept = 0, color = "black") + # Add x-axis line
  geom_vline(data = df_means, aes(xintercept = avg, color = as.factor(a)), linetype = "dashed", size = 1.2) + # Add average lines
  labs(title = "Medias con diferentes valores",
       x = "Valores X",
       y = "",
       color = "Media") + # Add labels for the legend
  ylim(0, 1) + # Set y-axis limits to show only above the x-axis
  scale_color_manual(values = c("red", "navy", "orange", "purple")) + # Define colors for lines
  theme_minimal() +
  theme(axis.text.y = element_blank(), # Remove y axis text
        axis.ticks.y = element_blank(), # Remove y axis ticks
        legend.position = "top", # Position legend at the top
        plot.title = element_text(size = 16, face = "bold"), # Title font size and style
        axis.title.x = element_text(size = 14), # X-axis title font size
        axis.text.x = element_text(size = 12), # X-axis text font size
        legend.text = element_text(size = 12), # Legend text font size
        legend.title = element_text(size = 14)) # Legend title font size


# Rango

numbers <- c(1, 2, 4, 4, 6, 8, 8, 8, 9, 11, 11, 12, 13)
# Generate the histogram
hist(numbers, breaks=c(0:14), main="Frecuencia", xlab="X", ylab="", col="maroon", border="black", ylim=c(0, 3))

numbers <- c(7,10,11,12,12,12)
# Generate the histogram
hist(numbers, breaks=c(0:14), main="Frecuencia",
     xlab="X", ylab="", col="maroon", border="black",
     ylim=c(0, 3), xlim=c(6, 12), cex.main=2.5)


# Hist - Desviación 
numbersA <- c(11,12,13,16,16,17,18,21)
numbersB <- c(14,15,15,15,16,16,16,17)
numbersC <- c(11,11,11,12,19,20,20,20)

# Plot histogram for numbersA
hist(numbersA, breaks = c(10:22), main = "Frecuencia A", xlab = "X", ylab = "", col = "maroon", border = "black", ylim = c(0, 3))

# Plot histogram for numbersB
hist(numbersB, breaks = c(10:22), main = "Frecuencia B", xlab = "X", ylab = "", col = "blue", border = "black", ylim = c(0, 3))

# Plot histogram for numbersC
hist(numbersC, breaks = c(10:22), main = "Frecuencia C", xlab = "X", ylab = "", col = "green", border = "black", ylim = c(0, 3))
abline(v = 15.5, col = "purple", lty = 2, lwd = 2) # lty=2 for dashed line, lwd=2 for line width

