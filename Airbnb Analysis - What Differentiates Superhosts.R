library(readxl)

# Read an Excel file
airbnb <- read_excel("data/AirbnbLA_2023--Cleaned.xlsx")


# Reshape the data
airbnb_long <- airbnb %>%
  pivot_longer(
    cols = c(ReviewScoresRating, ReviewScoresAccuracy, ReviewScoresCheckin, 
             ReviewScoresCleanliness, ReviewScoresCommunication, 
             ReviewScoresLocation, ReviewScoresValue),
    names_to = "ReviewMetric",
    values_to = "Score"
  )

# Part 1
# ---------------------------------------

# Just ReviewScoresRating
airbnb_long %>%
  filter(ReviewMetric == "ReviewScoresRating") %>%
  ggplot(aes(x = as.factor(HostIsSuperhost), y = Score)) +
  geom_boxplot() +
  labs(
    x = "Superhost Status",
    y = "Score",
    title = "Review Scores Rating by Superhost Status"
  ) +
  scale_x_discrete(labels = c("0" = "Non-Superhost", "1" = "Superhost"))

# Summarize quartiles for ReviewScoresRating
summary_table <- airbnb_long %>%
  filter(ReviewMetric == "ReviewScoresRating") %>%
  group_by(HostIsSuperhost) %>%
  summarize(
    Q1 = quantile(Score, 0.25, na.rm = TRUE),
    Median = quantile(Score, 0.50, na.rm = TRUE),
    Q3 = quantile(Score, 0.75, na.rm = TRUE)
  )

# View the table
summary_table

# Part 2
# ---------------------------------------

airbnb_long %>%
  filter(ReviewMetric != "ReviewScoresRating") %>%
  ggplot(aes(x = as.factor(HostIsSuperhost), y = Score)) +
  geom_boxplot() +
  facet_wrap(~ ReviewMetric, scales = "free_y") +
  labs(
    x = "Superhost Status",
    y = "Score",
    title = "Comparison of Review Scores by Superhost Status"
  ) + 
  theme(
    panel.spacing = unit(2, "lines")
  ) + 
  scale_x_discrete(labels = c("0" = "Non-Superhost", "1" = "Superhost"))




# Summarize quartiles for all review metrics
summary_table <- airbnb_long %>%
  group_by(ReviewMetric, HostIsSuperhost) %>%
  summarize(
    Q1 = quantile(Score, 0.25, na.rm = TRUE),
    Median = quantile(Score, 0.50, na.rm = TRUE),
    Q3 = quantile(Score, 0.75, na.rm = TRUE),
    .groups = "drop"  # Avoid grouped output
  )

# View the table
summary_table
