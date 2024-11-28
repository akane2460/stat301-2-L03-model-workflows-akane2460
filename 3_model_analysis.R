# L03 Model workflows & recipes ----
# model analysis

## load packages ----
library(tidyverse)
library(tidymodels)
library(here)

## handle conflicts----
tidymodels_prefer()

## load data testing---
kc_test <- read_rds(here("data/kc_test.rds"))

# load trained models 
load(here("results/fit_lm.rda"))
load(here("results/fit_lasso.rda"))
load(here("results/fit_ridge.rda"))
load(here("results/fit_rf.rda"))

# inspecting model fit (for regression model)
tidy_fit_lm <- tidy(fit_lm)

tidy_fit_lasso <- tidy(fit_lasso)

tidy_fit_ridge <- tidy(fit_ridge)

# save tidy outputs
write_rds(tidy_fit_lm, file = here("results/tidy_fit_lm.rds"))

write_rds(tidy_fit_lasso, file = here("results/tidy_fit_lasso.rds"))

write_rds(tidy_fit_ridge, file = here("results/tidy_fit_ridge.rds"))

# calculate RMSE
rsme_lm <- bind_cols(kc_test, predict(fit_lm, kc_test)) |> 
  select(price_log10, .pred) |> 
  mutate(difference = price_log10 - .pred)
    # adding in a variable here measuring the difference so it can be easier
    # to compare which model is best later

rsme_lasso <- bind_cols(kc_test, predict(fit_lasso, kc_test)) |> 
  select(price_log10, .pred) |> 
  mutate(difference = price_log10 - .pred)

rsme_ridge <- bind_cols(kc_test, predict(fit_ridge, kc_test)) |> 
  select(price_log10, .pred) |> 
  mutate(difference = price_log10 - .pred)

rsme_rf <- bind_cols(kc_test, predict(fit_rf, kc_test)) |> 
  select(price_log10, .pred) |> 
  mutate(difference = price_log10 - .pred)

# save RMSE outputs
write_rds(rsme_lm, file = here("results/rsme_lm.rds"))

write_rds(rsme_lasso, file = here("results/rsme_lasso.rds"))

write_rds(rsme_ridge, file = here("results/rsme_ridge.rds"))

write_rds(rsme_rf, file = here("results/rsme_rf.rds"))

# typical differences for each model
rsme_diffs <- tibble(rsme_lm |> summarize(lm_mean_diff = mean(difference)),
       rsme_lasso |> summarize(lasso_mean_diff = mean(difference)),
       rsme_ridge |> summarize(ridge_mean_diff = mean(difference)),
       rsme_rf |> summarize(rf_mean_diff = mean(difference)))

write_rds(rsme_diffs, file = here("results/rsme_diffs.rds"))
