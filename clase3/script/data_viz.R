library(tidyverse)
library(ggplot2movies)
library(gapminder)


# Movies
# Bars
movies <- movies
top1000 <- movies %>%
  arrange(desc(budget)) %>%
  slice_head(n = 1000)
top1000 <- top1000 %>%
  mutate(mpaa = ifelse(mpaa == "", "NA", mpaa))


top1000 %>%
  count(mpaa) %>%
  arrange(n) %>%
  mutate(mpaa = fct_inorder(mpaa)) %>%
  ggplot(aes(x = n, y = mpaa)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - 5), 
    color = "white", 
    size = 5, 
    hjust = 1
  ) +
  cowplot::theme_minimal_vgrid(16) +
  theme(
    axis.title.y = element_blank(), 
    legend.position = "none" 
  ) +
  xlab("")


top1000 %>%
  count(mpaa) %>%
  arrange(n) %>%
  mutate(mpaa = fct_inorder(mpaa)) %>%
  ggplot(aes(x = n, y = mpaa)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - 5), 
    color = "white", 
    size = 5, 
    hjust = 1
  ) +
  cowplot::theme_minimal_vgrid(16) +
  theme(
    axis.title.x = element_text(size = 14),  # Set x-axis label size and font
    axis.title.y = element_text(size = 14),  # Set y-axis label size and font
    legend.position = "none"
  ) +
  xlab("Número de películas") +
  ylab("Categoría (Motion Picture Association)")

top1000 %>%
  count(mpaa) %>%
  arrange(n) %>%
  mutate(mpaa = fct_inorder(mpaa)) %>%
  ggplot(aes(x = n, y = mpaa)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - 5), 
    color = "white", 
    size = 5, 
    hjust = 1
  ) +
  cowplot::theme_minimal_vgrid(16) +
  theme(
    axis.title.x = element_text(size = 14),  # Set x-axis label size and font
    axis.title.y = element_text(size = 12, angle = 0, vjust = 0.5, hjust = 0.5),  # Set y-axis label as subtitle
    legend.position = "none",
    plot.title = element_text(size = 14),  # Optional: adjust plot title size
    plot.subtitle = element_text(size = 12, face = "italic")  # Optional: add subtitle formatting if needed
  ) +
  ggtitle("Categoría (Motion Picture Association)") +
  xlab("Número de películas") +
  ylab("")

movies_mpaa <- top1000 %>%
  count(mpaa) %>%
  arrange(n) %>%
  mutate(
    pg13 = ifelse(mpaa == "PG-13", TRUE, FALSE),
    mpaa = fct_inorder(mpaa)
  )

movies_mpaa %>%
  ggplot(aes(x = n, y = mpaa, fill = pg13)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - 5), 
    color = "white", 
    size = 5, 
    hjust = 1
  ) +
  cowplot::theme_minimal_vgrid(16) +
  theme(
    axis.title.y = element_blank(), 
    legend.position = "none"
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#B0B0B0D0", "#D55E00D0")
  ) + ggtitle("Categoría (Motion Picture Association)") +
  xlab("Número de películas") +
  ylab("")


movies_summary <- top1000 %>%
  group_by(year, mpaa) %>%
  summarize(count = n(), .groups = 'drop')

  ggplot(movies_summary, aes(x = year, y = count, color = mpaa)) +
  geom_line() +
  labs(title = "Number of Films in Each MPAA Category Over Time",
       x = "Year",
       y = "Number of Films",
       color = "MPAA Rating") +
  theme_minimal()
  
  
  movies_summary <- top1000 %>%
    group_by(year, mpaa) %>%
    summarize(avg_budget = mean(budget, na.rm = TRUE), .groups = 'drop')
  
  ggplot(movies_summary, aes(x = year, y = avg_budget/1000000, color = mpaa)) +
    geom_line() +
    labs(title = "Average Budget for Each MPAA Category Over Time",
         x = "Year",
         y = "Average Budget",
         color = "MPAA Rating") +
    theme_minimal()

  
  # Gapminder
  # No legends
  gapminder <- gapminder
  gap_continent <- gapminder %>%
    group_by(year, continent) %>%
    summarize(lifeExp = mean(lifeExp, na.rm = TRUE), .groups = 'drop')
  
  ggplot(gap_continent, aes(x = year, y = lifeExp, color = continent)) +
    geom_line() +
    labs(title = "Average Life Expectancy by Continent",
         x = "Year",
         y = "",
         color = "Continent") +
    theme_minimal()
  
  
  gap_continent_summary <- gap_continent %>%
    group_by(continent) %>%
    filter(year == max(year)) %>%
    ungroup()
  # Custom color mapping
  continent_colors <- c("Africa" = "#D55E00D0", "Europe" = "navy", 
                        "Asia" = "navy", "Americas" = "navy", 
                        "Oceania" = "navy")
  
  ggplot(gap_continent, aes(x = year, y = lifeExp, group = continent)) +
    geom_line(aes(color = continent)) +
    geom_text(data = gap_continent_summary, aes(label = continent), 
              hjust = -0.2, vjust = 0, size = 5.5, color = "black") +
    labs(title = "Average Life Expectancy by Continent",
         x = "Year",
         y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(color = "black")) +
    scale_color_manual(values = continent_colors) +
    xlim(min(gap_continent$year), max(gap_continent$year) + 2)
  
  
  ggplot(gap_continent, aes(x = year, y = lifeExp, group = continent)) +
    geom_line(aes(color = continent)) +
    geom_text(data = gap_continent_summary, aes(label = continent), 
              hjust = -0.2, vjust = 0, size = .5, color = "black") +
    labs(title = "Average Life Expectancy by Continent",
         x = "Year",
         y = "Life Expectancy") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          plot.margin = margin(5.5, 30, 5.5, 5.5)) + # Increase right margin
    scale_color_manual(values = continent_colors) +
    xlim(min(gap_continent$year), max(gap_continent$year) + 10)  # Add space on the right
  
  
  # Grepel
  library(ggrepel)
  gapminder %>%
    filter(year == 2007) %>%
    ggplot(aes(log(gdpPercap), lifeExp)) +
    geom_point(
      size = 3.5, 
      alpha = .9, 
      shape = 21, 
      col = "white", 
      fill = "#0162B2"
    ) +
    geom_text_repel(aes(label = country)) +
    theme_minimal(14) +
    theme(panel.grid.minor = element_blank()) +
    labs(
      x = "log(GDP per capita)",
      y = "life expectancy"
    )
  
  
  south_american_countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                                "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela")

  p1 <- gapminder %>%
    filter(year == 2007) %>%
    mutate(
      label = ifelse(
        country  %in% south_american_countries,
        as.character(country),
        ""
      )
    ) %>%
    ggplot(aes(log(gdpPercap), lifeExp)) +
    geom_point(
      size = 3.5, 
      alpha = .9, 
      shape = 21, 
      col = "white", 
      fill = "#0162B2"
    )  
  
  scatter_plot <- p1 + 
    geom_text_repel(
      aes(label = label),
      size = 4.5,
      point.padding = .2,
      box.padding = .3,
      force = 1,
      min.segment.length = 0,
      max.overlaps = 35  # Increase this number as needed
    ) +
    theme_minimal(14) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "log(GDP per capita)",
      y = "life expectancy"
    )
  
  scatter_plot
  