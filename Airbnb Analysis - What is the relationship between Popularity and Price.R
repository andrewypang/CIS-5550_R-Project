library(readxl)

# Read an Excel file
airbnb <- read_excel("data/AirbnbLA_2023--Cleaned.xlsx")

# Are Superhost listings more popular and pricer?

airbnb %>%
  ggplot(aes(x = Price, y = NumberOfReviews)) +
  geom_point(
    aes(color = as.factor(HostIsSuperhost)), 
    alpha = 0.4
  ) +
  geom_smooth(
    aes(linetype = as.factor(HostIsSuperhost)), 
    se = FALSE
  ) +
  scale_color_discrete(
    name = "Superhost Status", 
    labels = c("0" = "Non-Superhost", "1" = "Superhost")
  ) +
  scale_linetype_discrete(
    name = "Superhost Status", 
    labels = c("0" = "Non-Superhost", "1" = "Superhost")
  ) +
  labs(
    x = "Price",
    y = "Number of Reviews",
    title = "Price vs. Popularity (Reviews) by Superhost Status"
  ) +
  theme_minimal()
