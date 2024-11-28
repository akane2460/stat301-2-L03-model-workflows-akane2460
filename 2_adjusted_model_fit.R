# L03 Model workflows & recipes ----
# defining new workflow

## load packages ----
library(tidyverse)
library(tidymodels)
library(rsample)
library(here)
library(splines)

## handle conflicts----
tidymodels_prefer()

## load data training----
kc_train <- read_rds(here("data/kc_train.rds"))

## Ex 4----

## task 1----
# sqft_lot
kc_train |> 
  ggplot(aes(price_log10, sqft_lot)) +
  geom_point()
    # log vs log scale would be better

sqft_lot_vis <- kc_train |> 
  ggplot(aes(price_log10, log(sqft_lot))) +
  geom_point()

# lat
kc_train |> 
  ggplot(aes(price_log10, lat)) +
  geom_point()

lat_vis <- kc_train |> 
  ggplot(aes(price_log10, lat)) +
  geom_point(alpha = .2) +
  geom_smooth(
    method = "lm",
    formula = y ~ ns(x, df = 5), 
    se = FALSE,
    color = "red"
  )

ggsave(here("results/lat_vis.png"), lat_vis)
ggsave(here("results/sqft_lot_vis.png"), sqft_lot_vis)

## task 2
kc_recipe_adjusted <- recipe(
  price_log10 ~ waterfront + sqft_living + sqft_lot + lat + bedrooms + date,
  data = kc_train) |> 
  step_log(sqft_lot) |> 
  step_ns(lat, deg_free = 5) |> 
  step_date(date, features = "month") |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  update_role(date, new_role = "id")

# check recipe
# kc_recipe_adjusted |>
#   prep() |>
#   bake(new_data = NULL)

# write recipe out
save(kc_recipe_adjusted, file = here("results/kc_recipe_adjusted.rda"))

## task 3

# model specification
# OLS
lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# define workflow
lm_wflow_adjusted <-
  workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(kc_recipe_adjusted)

# fit workflow
fit_lm_adjusted <- fit(lm_wflow_adjusted, kc_train)

# save adjusted workflow
save(fit_lm_adjusted, file = here("results/fit_lm_adjusted.rda"))


