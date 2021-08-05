# MODELTIME ITERATIVE (NESTED) FORECASTING ----


# NOTES ----
# * Will the Nested Modeltime Table get too big?
#   - Yes it will when N-models x n-series becomes large
#   - Solution: Use Modeltime Targets for >> RAM. Under development ----
# * Speed:
#   - Our solution has speed limitations
#   - We can parallelize to make faster, evenly distributing workload by time series ID
# * What about parsnip models?  ----
#   - Thinking we should enforce workflows


library(tidymodels)
library(timetk)
library(modeltime)
library(tidyverse)

source("R/01_data_prep.R")
source("R/02_nested_modeltime.R")

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

    # >> Can add xregs in here <<

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

# * XGBoost ----

rec_xgb <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]])) %>%
    step_timeseries_signature(date) %>%
    step_rm(date) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
    add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
    add_recipe(rec_xgb)

wflw_xgb

wflw_xgb_fit <- wflw_xgb %>% fit(training(nested_data_tbl$.splits[[1]]))

wflw_xgb_fit

# * ARIMA ----

recipe_arima <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]]))

wflw_arima <- workflow() %>%
    add_model(arima_reg() %>% set_engine("auto_arima")) %>%
    add_recipe(recipe_arima)

wflw_arima

# * Bad Model ----
#   - Xgboost can't handle dates

recipe_bad <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]]))

wflw_bad <- workflow() %>%
    add_model(boost_tree()) %>%
    add_recipe(recipe_arima)

wflw_bad

# * Prophet ----

wflw_prophet <- workflow() %>%
    add_model(prophet_reg(seasonality_yearly = TRUE)) %>%
    add_recipe(recipe_arima)

# NESTED WORKFLOW ----

# * Nested Modeltime Table
#   - Works with
nested_modeltime_tbl <- nested_data_tbl %>%
    modeltime_nested_fit(
        wflw_arima,
        wflw_xgb_fit, # FITTED WORKS
        wflw_bad,     # BAD MODEL RESULTS IN ERROR LOGGING
        wflw_prophet,
        control = control_nested_fit(verbose = TRUE)
    )

nested_modeltime_tbl

# * Attributes ----
#   - Logs key results: accuracy table, test forecast table
#   - Pushes expensive computations to modeling
#   - Speeds up evaluation

attributes(nested_modeltime_tbl)

nested_modeltime_tbl %>% modeltime_nested_accuracy()

nested_modeltime_tbl %>% modeltime_nested_error_report()

nested_modeltime_tbl %>%
    modeltime_nested_test_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol = 2
    )

# * Object size can get large | Limited by RAM ----
#   - 7 time series x 3 models = 10 MB
#   - Realistically, we can only use for ~ 10,000 time series (10000/7 * 10 * 1e-3 = 14.3GB)
#   - Have a good solution using Targets package

object.size(nested_modeltime_tbl)


# SELECT BEST ----

best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
    modeltime_nested_select_best()

best_nested_modeltime_tbl %>%
    modeltime_nested_best_model_report()

best_nested_modeltime_tbl %>%
    modeltime_nested_test_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(.facet_ncol = 2)


# REFIT / FUTURE FORECAST ----

nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(control = control_nested_refit(verbose = FALSE))

attributes(nested_modeltime_refit_tbl)

nested_modeltime_refit_tbl %>%
    modeltime_nested_future_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol = 2
    )

