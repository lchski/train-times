library(tidyverse)
library(lubridate)

library(ggthemr)
ggthemr("grape")

# Import
arrivals <- read_csv("data/84.csv")

arrivals <- arrivals %>%
  filter(! is.na(Actual)) %>%
  transmute(
    date = Date,
    scheduled = Scheduled,
    actual = Actual,
    diff = Differential,
    weekday = wday(date, week_start = 1, label = TRUE, abbr = FALSE),
    week = week(date),
    week_start = ymd(paste0(year(date), "-01-01")) + weeks(week - 1)
  )


# Summarize
daily_report <- arrivals %>%
  group_by(weekday) %>%
  summarize(min = min(diff), max = max(diff), avg = round(mean(diff), digits = 0), count = n())

weekly_report <- arrivals %>%
  group_by(week, week_start) %>%
  summarize(min = min(diff), max = max(diff), avg = round(mean(diff), digits = 0), count = n())


# Visualize

## Average difference between scheduled and actual arrival time by week
weekly_report %>% ggplot(mapping = aes(x = week_start, y = avg)) +
  geom_point() +
  geom_smooth()

## Difference between scheduled and actual arrival time by day
arrivals %>% ggplot(mapping = aes(x = date, y = diff)) +
  geom_point() +
  geom_smooth()

## Difference between scheduled and actual arrival time by day of the week
arrivals %>% ggplot(mapping = aes(x = weekday, y = diff)) +
  geom_boxplot() +
  labs(
    title = "Difference between scheduled and actual arrival in Toronto\nfor VIA Train 84",
    caption = "Data from viarail.ca"
  ) +
  xlab("Day of the week") +
  ylab("Difference (minutes)") +
  theme(
    text = element_text(family = "Helvetica")
  )
