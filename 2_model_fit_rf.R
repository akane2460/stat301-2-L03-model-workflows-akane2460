# L03 Model workflows & recipes ----
# defining model: random forest

## load packages ----
library(tidyverse)
library(tidymodels)
library(rsample)
library(here)
library(ranger)

## handle conflicts----
tidymodels_prefer()

## load data training----
kc_train <- read_rds(here("data/kc_train.rds"))

## Ex 2 and 3 (part)----

# define preprocessing/feature engineering

kc_recipe <- recipe(
  price_log10 ~ waterfront + sqft_living + yr_built + bedrooms,
  data = kc_train) |> 
  step_dummy(waterfront)

# check recipe
# kc_recipe |> 
#   prep() |> 
#   bake(new_data = NULL)

# model specification
rf_spec <- rand_forest(trees = 500, min_n = 5) |> 
  set_engine("ranger") |> 
  set_mode("regression")

# define workflows
rf_wflow <-
  workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(kc_recipe)

# fit workflows
fit_rf <- fit(rf_wflow, kc_train)

# write out fits
save(fit_rf, file = here("results/fit_rf.rda"))



