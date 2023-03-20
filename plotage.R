library(ggplot2)
library(dplyr)

# Load data
data <- read.csv("psfd_rr2020_v202208_csv.csv", stringsAsFactors = FALSE)
head(data$w03, n = 10)
# work dummy
data$work <- ifelse(data$w03 == 3, 0, 1)

# Compute age
data$age <- 2020 - (1911 + data$a02a) + 1

# Create a new data frame with the mean of work dummy for each age
df <- data %>%
  group_by(age) %>%
  summarize(mean_work = mean(work))

# Plot the rate of working against age
ggplot(df, aes(x = mean_work, y = age)) +
  geom_point() +
  labs(x = "Mean of work dummy", y = "Age") +
  theme_bw()
