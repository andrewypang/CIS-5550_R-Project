library(readxl)

# Read an Excel file
airbnb <- read_excel("data/AirbnbLA_2023--Cleaned.xlsx")

# What Types of Listings Do Superhost Offer?
# CHART
ggplot(airbnb, aes(x=as.factor(HostIsSuperhost))) +
  geom_bar(aes(fill=RoomType)) +
  scale_x_discrete(
    name = "Superhost Status",  # Legend title
    labels = c("0" = "Not Superhost", "1" = "Superhost")  # Custom labels
  ) +
  labs(
    y="Number of Listings",
    title = "What Types of Listings Do Superhost/Non-Superhost Offer?"
  )

# TABLE
airbnb %>%
  group_by(HostIsSuperhost, RoomType) %>%
  summarize(
    Count = n(),
    .groups = "drop_last"  # Retain grouping for percentage calculation
  ) %>%
  mutate(
    Total = sum(Count),
    Percent = round((Count / Total) * 100, 2)  # Retain numeric type and round to 2 decimal places
  ) %>%
  ungroup() %>%
  print()

# ------------
# On average, do Superhosts/Non-Superhosts list multiple properties? 
airbnb %>%
  group_by(HostId, HostIsSuperhost) %>%  # Group by HostId and Superhost status
  summarize(ListingCount = n(), .groups = "drop") %>%  # Count listings per host
  group_by(HostIsSuperhost) %>%  # Group by Superhost status
  summarize(
    AvgListings = mean(ListingCount, na.rm = TRUE),  # Calculate average listings per host
    TotalHosts = n()  # Total number of hosts in each group
  ) %>%
  print() %>%
  ggplot(aes(x=as.factor(HostIsSuperhost),y=AvgListings)) + geom_col()
  
# # On average, do Superhosts/Non-Superhosts list multiple properties disaggregated by Room Type
airbnb %>%
  group_by(HostId, HostIsSuperhost, RoomType) %>%  # Group by HostId, Superhost status, and neighborhood
  summarize(ListingCount = n(), .groups = "drop") %>%  # Count listings per host per neighborhood
  group_by(HostIsSuperhost, RoomType) %>%  # Group by Superhost status and neighborhood
  summarize(
    AvgListings = mean(ListingCount, na.rm = TRUE),  # Calculate average listings per host
    TotalHosts = n(),  # Total number of hosts in each group
    .groups = "drop"  # Drop grouping for a clean output
  ) %>%
  print() %>%
  ggplot(aes(x=as.factor(HostIsSuperhost),y=AvgListings)) +
  geom_col() +
  facet_grid(~ RoomType) +
  scale_x_discrete(
    name = "Superhost Status",  # X-axis label
    labels = c("0" = "Not Superhost", "1" = "Superhost")  # Custom labels
  ) +
  labs(
    y="Average Number of Listings per Host",
    title = "Average Number of Listings per Host disaggregated by Room Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
