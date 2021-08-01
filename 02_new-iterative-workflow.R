# MODELTIME ITERATIVE FORECAST ----
# -


library(tidymodels)
library(timetk)
library(modeltime)
library(tidyverse)

source("R/01_data_prep.R")


# DATA PREP FUNCTIONS ----

nested_data_tbl <- walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    set_names(c("id", "date", "value")) %>%

    extend_timeseries(
        .id_var     = id,
        .date_var   = date,
        .value      = value,
        .length_out = 52
    ) %>%

    nest_timeseries(
        .id_var   = id,
        .date_var = date,
        .value    = value
    ) %>%

    split_nested_timeseries(
        .length_test = 52
    )

nested_data_tbl

# MODELING ----

# * Xgboost ----

rec_xgb <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]])) %>%
    step_timeseries_signature(date) %>%
    step_rm(date) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
    add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
    add_recipe(rec_xgb)

wflw_xgb

# * ARIMA ----

recipe_arima <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]]))

wflw_arima <- workflow() %>%
    add_model(arima_reg() %>% set_engine("auto_arima")) %>%
    add_recipe(recipe_arima)

wflw_arima

# * Bad Model ----

recipe_bad <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]]))

wflw_bad <- workflow() %>%
    add_model(boost_tree()) %>%
    add_recipe(recipe_arima)

wflw_bad

# ITERATIVE WORKFLOW ----

# * Nested Modeltime Table
nested_modeltime_tbl <- nested_data_tbl %>%
    modeltime_nested_fit(
        wflw_arima,
        wflw_xgb,
        wflw_bad
    )

nested_modeltime_tbl

attributes(nested_modeltime_tbl)
attr(nested_modeltime_tbl, "id")
attr(nested_modeltime_tbl, "error_tbl")
attr(nested_modeltime_tbl, "accuracy_tbl")

nested_modeltime_tbl %>%
    modeltime_nested_accuracy()
