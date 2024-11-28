# L03 Model workflows & recipes ----
# model analysis

## load packages ----
library(tidyverse)
library(tidymodels)
library(here)

## handle conflicts----
tidymodels_prefer()

# load data testing---
kc_test <- read_rds(here("data/kc_test.rds"))

## Ex 4----

## task 3----
# load trained models 
load(here("results/fit_lm_adjusted.rda"))

# inspecting model fit (for regression model)
tidy_fit_lm_adjusted <- tidy(fit_lm_adjusted)

# save tidy outputs
write_rds(tidy_fit_lm_adjusted, file = here("results/tidy_fit_lm_adjusted.rds"))

# calculate RMSE
rsme_lm_adjusted <- bind_cols(kc_test, predict(fit_lm_adjusted, kc_test)) |> 
  select(price_log10, .pred) |>
  mutate(difference = price_log10 - .pred)

write_rds(rsme_lm_adjusted, file = here("results/rsme_lm_adjusted.rds"))

## task 4----

# What proportion or percentage of the predicted prices are within 25% of the original price on the testing set? 
# Is this value surprising or not surprising to you? Explain.

rsme_lm_adjusted |> 
  mutate(upper_bound = 1.25 * .pred,
         lower_bound = .75 * .pred, 
         within = if_else(
           price_log10 >= lower_bound|price_log10 >= lower_bound,
           TRUE,
           FALSE)) |> 
  filter(within == FALSE)

