library(readxl)

# Read an Excel file
airbnb <- read_excel("data/AirbnbLA_2023--Cleaned.xlsx")

# How Does Responsiveness Factor In?

ggplot(airbnb, aes(x = as.factor(HostIsSuperhost), fill = HostResponseTime)) + 
  geom_bar(position = "dodge") +  # Use dodge to separate bars by HostResponseTime
  scale_x_discrete(labels = c("0" = "Not Superhost", "1" = "Superhost")) +
  labs(
    x = "Superhost Status",
    y = "Number of Hosts",
    title = "Host Response Time by Superhost Status",
    fill = "Response Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# TABLE
airbnb %>%
  group_by(HostIsSuperhost, HostResponseTime) %>%  # Group by HostId and Superhost status
  summarize(HostCount = n(), .groups = "drop_last") %>%  # Count listings per host
  mutate(
    Total = sum(HostCount),
    Percent = round((HostCount / Total) * 100, 2)  # Retain numeric type and round to 2 decimal places
  ) %>%
  ungroup() %>%
  print()
