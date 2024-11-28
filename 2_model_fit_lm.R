# L03 Model workflows & recipes ----
# defining model: linear

## load packages ----
library(tidyverse)
library(tidymodels)
library(rsample)
library(here)

## handle conflicts----
tidymodels_prefer()

## load data training----
kc_train <- read_rds(here("data/kc_train.rds"))

## Ex 2 and 3----

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

# OLS
lm_spec <-linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# ridge
ridge_spec <-linear_reg(penalty = 0.01, mixture = 0) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# lasso
lasso_spec <-linear_reg(penalty = 0.01, mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# define workflows
lm_wflow <-
  workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(kc_recipe)

ridge_wflow <-
  workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(kc_recipe)

lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(kc_recipe)

# fit workflows

fit_lm <- fit(lm_wflow, kc_train)

fit_ridge <- fit(ridge_wflow, kc_train)

fit_lasso <- fit(lasso_wflow, kc_train)

# write out fits

save(fit_lm, file = here("results/fit_lm.rda"))

save(fit_ridge, file = here("results/fit_ridge.rda"))

save(fit_lasso, file = here("results/fit_lasso.rda"))



