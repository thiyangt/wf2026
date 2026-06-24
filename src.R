pak::pak("thiyangt/denguedatahub")
library(denguedatahub)
library(tidyverse)
library(ranger)

data("srilanka_weekly_data")

data <- srilanka_weekly_data

# ----------------------------
# Create lag features (initial)
# ----------------------------
for (i in 1:8) {
  data <- data %>%
    group_by(district) %>%
    mutate(!!paste0("lag_", i) := lag(cases, i)) %>%
    ungroup()
}

data <- data %>%
  group_by(district) %>%
  mutate(lag_52 = lag(cases, 52)) %>%
  ungroup()

# ----------------------------
# Train / Test split
# ----------------------------
train <- data %>% filter(year < 2026)
test  <- data %>% filter(year == 2026)

# ----------------------------
# Train model
# ----------------------------
model <- ranger(
  cases ~ district + lag_1 + lag_2 + lag_3 + lag_4 +
    lag_5 + lag_6 + lag_7 + lag_8 + year + week,
  data = train,
  num.trees = 1000,
  importance = "permutation"
)

# ----------------------------
# Recursive forecasting
# ----------------------------
data_full <- data
forecast_results <- list()

for (w in 1:4) {
  
  test_w <- data_full %>%
    filter(year == 2026, week == w)
  
  pred <- predict(model, data = test_w)$predictions
  
  test_w <- test_w %>%
    mutate(pred_cases = pred)
  
  # store forecast
  forecast_results[[w]] <- test_w
  
  # append predicted values into history
  data_full <- data_full %>%
    bind_rows(
      test_w %>%
        mutate(cases = pred) %>%
        select(year, week, start.date, end.date, district, cases)
    )
  
  # recompute lags after update
  for (i in 1:8) {
    data_full <- data_full %>%
      group_by(district) %>%
      mutate(!!paste0("lag_", i) := lag(cases, i)) %>%
      ungroup()
  }
}

# ----------------------------
# Final forecast table
# ----------------------------
forecast_results <- bind_rows(forecast_results)
colnames(library(denguedatahub)
library(tidyverse)
library(ranger)

data("srilanka_weekly_data")

data <- srilanka_weekly_data

# ----------------------------
# Create lag features (initial)
# ----------------------------
for (i in 1:8) {
  data <- data %>%
    group_by(district) %>%
    mutate(!!paste0("lag_", i) := lag(cases, i)) %>%
    ungroup()
}

data <- data %>%
  group_by(district) %>%
  mutate(lag_52 = lag(cases, 52)) %>%
  ungroup()

# ----------------------------
# Train / Test split
# ----------------------------
train <- data %>% filter(year < 2026)
test  <- data %>% filter(year == 2026)

# ----------------------------
# Train model
# ----------------------------
model <- ranger(
  cases ~ district + lag_1 + lag_2 + lag_3 + lag_4 +
    lag_5 + lag_6 + lag_7 + lag_8 + year + week,
  data = train,
  num.trees = 1000,
  importance = "permutation"
)

# ----------------------------
# Recursive forecasting
# ----------------------------
data_full <- data
forecast_results <- list()

for (w in 1:4) {

  test_w <- data_full %>%
    filter(year == 2026, week == w)

  pred <- predict(model, data = test_w)$predictions

  test_w <- test_w %>%
    mutate(pred_cases = pred)

  # store forecast
  forecast_results[[w]] <- test_w

  # append predicted values into history
  data_full <- data_full %>%
    bind_rows(
      test_w %>%
        mutate(cases = pred) %>%
        select(year, week, start.date, end.date, district, cases)
    )

  # recompute lags after update
  for (i in 1:8) {
    data_full <- data_full %>%
      group_by(district) %>%
      mutate(!!paste0("lag_", i) := lag(cases, i)) %>%
      ungroup()
  }
}

# ----------------------------
# Final forecast table
# ----------------------------
forecast_results <- bind_rows(forecast_results))
colnames(forecast_results)
View(forecast_results)

############
forecast_results <- forecast_results %>%
  mutate(
    pct_change = ifelse(lag_1 == 0, NA, (pred_cases - lag_1) / lag_1 * 100)
  )

forecast_results <- forecast_results %>%
  mutate(
    change_category = case_when(
      pct_change < -10 ~ "Decrease",
      pct_change >= -10 & pct_change <= 20 ~ "Stable",
      pct_change > 20 & pct_change <= 50 ~ "Moderate Increase",
      pct_change > 50 & pct_change <= 100 ~ "Sharp Increase",
      pct_change > 100 & pct_change <= 200 ~ "Very Sharp Increase",
      pct_change > 200 ~ "Outbreak Alert",
      TRUE ~ NA_character_
    )
  )

forecast_results$change_category <- factor(
  forecast_results$change_category,
  levels = c(
    "Decrease",
    "Stable",
    "Moderate Increase",
    "Sharp Increase",
    "Very Sharp Increase",
    "Outbreak Alert"
  )
)

lm_model <- lm(
  cases ~ pred_cases + change_category,
  data = forecast_results
)

summary(lm_model)

###### Prediction


