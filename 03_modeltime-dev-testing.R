# MODELTIME ITERATIVE (NESTED) FORECASTING ----


# Requires the modeltime_nested branch until merge
remotes::install_github("business-science/modeltime@modeltime_nested")

library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)



# DATA PREP FUNCTIONS ----

nested_data_tbl <- walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    set_names(c("id", "date", "value")) %>%

    extend_timeseries(
        .id_var     = id,
        .date_var   = date,
        .length_out = 52
    ) %>%

    # >> Can add xregs in here <<

    nest_timeseries(
        .id_var     = id,
        .length_out = 52
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

# * ARIMA ----

recipe_arima <- recipe(value ~ ., training(nested_data_tbl$.splits[[1]]))

wflw_arima <- workflow() %>%
    add_model(arima_reg() %>% set_engine("auto_arima")) %>%
    add_recipe(recipe_arima)

wflw_arima

# * Prophet ----

wflw_prophet <- workflow() %>%
    add_model(prophet_reg(seasonality_yearly = TRUE)) %>%
    add_recipe(recipe_arima)

wflw_prophet


# NESTED WORKFLOW ----

# * Nested Modeltime Table
#   - Works with
nested_modeltime_tbl <- nested_data_tbl %>%
    modeltime_nested_fit(
        wflw_arima,
        wflw_xgb,
        wflw_prophet,

        control = control_nested_fit(verbose = TRUE)
    )

nested_modeltime_tbl

# * Attributes ----
#   - Logs key results: accuracy table, test forecast table
#   - Pushes expensive computations to modeling
#   - Speeds up evaluation


nested_modeltime_tbl %>% extract_nested_test_accuracy()

nested_modeltime_tbl %>% extract_nested_error_report()

nested_modeltime_tbl %>%
    extract_nested_test_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2,
        .interactive = F
    )


# SELECT BEST ----

best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
    modeltime_nested_select_best(metric = "rsq", minimize = FALSE)

best_nested_modeltime_tbl %>%
    extract_nested_best_model_report()

best_nested_modeltime_tbl %>%
    extract_nested_test_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2,
        .interactive = F
    )


# REFIT / FUTURE FORECAST ----

nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = FALSE)
    )

nested_modeltime_refit_tbl %>%
    extract_nested_future_forecast() %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .conf_interval_show = F,
        .facet_ncol         = 2,
        .interactive        = F
    )

