# MODELTIME ITERATIVE FORECAST ----


library(tidymodels)
library(timetk)
library(modeltime)
library(tidyverse)

# DATA ----
nested_data <- walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    set_names(c("id", "date", "value")) %>%
    nest(actual_data = -id)

nested_data

# Resample Strategy ----

nested_data <- nested_data %>%
    mutate(resamples = map(actual_data, .f = function(x) {
        time_series_split(x, assess = 52, cumulative = TRUE)
    }))

nested_data

# Future Strategy ----

nested_data <- nested_data %>%
    mutate(
        future_data = map(actual_data, .f = function(x) {
            future_frame(x, .length_out = 52)
        })
    )


# DATA FUNCTION ----

.data <- walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    set_names(c("id", "date", "value")) %>%
    group_by(id) %>%
    future_frame(.length_out = 52, .bind_data = TRUE) %>%
    ungroup()

extend_timeseries <- function(.data, .id_var, .date_var, .value, .length_out) {

    val_expr <- enquo(.value)
    id_expr <- enquo(.id_var)

    if (rlang::quo_is_missing(val_expr)) rlang::abort("`.value` is missing with no default. This should be a target variable.")
    if (rlang::quo_is_missing(id_expr)) rlang::abort("`.id_var` is missing with no default. This should be a column that identifies time series groupings.")

    # CHECKS

    missing_data_tbl <- .data %>%
        filter(is.na(!! val_expr))

    if (nrow(missing_data_tbl) > 0) {
        col_name <- quo_name(val_expr)
        cli::cli_h2("Missing Target Value Report:")
        cli::cli_alert_danger(str_glue("The following data has missing values in the `{col_name}` column."))
        print(missing_data_tbl)
        rlang::abort(
            str_glue("Missing data detected in `{col_name}` .value column. Please fix by filling missing values.")
        )
    }

    # EXTEND

    .data %>%
        group_by(!! enquo(.id_var)) %>%
        future_frame(
            .date_var   = !! enquo(.date_var),
            .length_out = .length_out,
            .bind_data  = TRUE
        ) %>%
        ungroup()
}

walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    set_names(c("id", "date", "value")) %>%
    mutate(value = NA) %>%
    extend_timeseries(.id_var = id, .value = value, .length_out = 52)

nest_timeseries <- function(.data, .id_var, .date_var, .value) {

    id_var_expr    <- enquo(.id_var)
    date_var_expr  <- enquo(.date_var)
    value_var_expr <- enquo(.value)

    # SPLIT FUTURE AND ACTUAL DATA

    actual_data_tbl <- .data %>%
        filter(!is.na( !!value_var_expr ))

    future_data_tbl <- .data %>%
        filter(is.na( !!value_var_expr))


    # CHECKS
    if (nrow(future_data_tbl) == 0) {
        rlang::warn("Future Data is `NULL`. Try using `extend_timeseries()` to add future data.")
    }

    # NEST

    ret_1 <- actual_data_tbl %>%
        nest(.actual_data = - (!! id_var_expr))

    ret_2 <- future_data_tbl %>%
        nest(.future_data = - (!! id_var_expr))

    # JOIN

    id_col_text <- names(ret_1)[[1]]

    ret <- left_join(ret_1, ret_2, by = id_col_text)

    return(ret)


}



split_timeseries <- function(.data, .length_test, .length_train = NULL, ...) {

    if (rlang::is_missing(.length_test)) rlang::abort("`.length_test` is missing. Provide a value for the time series length of the test split. ")

    if (!".actual_data" %in% names(.data)) rlang::abort("`.actual_data` column is not found. Try using `nest_timeseries()` to create a nested data frame with columns `.actual_data` and `.future_data`.")

    cum <- FALSE
    if (is.null(.length_train)) {
        cum <- TRUE
        .length_train <- 5
    }

    suppressMessages({
        .data %>%
            mutate(.splits = map(.actual_data, .f = function(x) {
                time_series_split(
                    x,
                    initial    = .length_train,
                    assess     = .length_test,
                    cumulative = cum,
                    ...
                )
            }))
    })

}


data_prep_tbl <- walmart_sales_weekly %>%
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
    split_timeseries(
        .length_test = 52
    )

# MODELING ----

# Workflow (Recipe + Model Spec) ----

rec_xgb <- recipe(value ~ ., training(nested_data$resamples[[1]])) %>%
    step_timeseries_signature(date) %>%
    step_rm(date) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
    add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
    add_recipe(rec_xgb)

wflw_xgb


recipe_arima <- recipe(value ~ ., training(nested_data$resamples[[1]]))

wflw_arima <- workflow() %>%
    add_model(arima_reg() %>% set_engine("auto_arima")) %>%
    add_recipe(recipe_arima)

wflw_arima

# Fitting Modeltime Table ----

nested_modeltime <- nested_data %>%
    mutate(
        modeltime_tables = pmap(.l = list(x = resamples, id = id), .f = function(x, id) {

            tryCatch({
                cli::cli_alert_info("Starting Modeltime Table: ID {id}...")

                # stop("model failed")

                model_list <- list(wflw_xgb, wflw_arima)

                ret <- model_list %>%
                    # NOTE - If one model fails, then all will fail ----
                    # Can be adjusted later.
                    imap(.f = function (mod, i) {

                        # Use this to test a model failure:
                        # if (i == 2 && id == "1_1") stop("Model failed")

                        ret <- fit(mod, data = training(x))

                        return(ret)

                    }) %>%
                    as_modeltime_table() %>%
                    modeltime_calibrate(testing(x))

                cli::cli_alert_success("Finished Modeltime Table: ID {id}")
                cat("\n")

            }, error = function(e) {

                cli::cli_alert_danger(str_glue("Modeltime Table (Failed): ID {id}"))
                cli::cli_inform(e)
                cat("\n")

                ret <- NULL

            })

            return(ret)
        })
    )

nested_modeltime

# ACCURACY ----

nested_accuracy <- nested_modeltime %>%
    mutate(
        accuracy_tables = pmap(list(modeltime_tables, id), .f = function(x, i) {

            tryCatch({
                # cli::cli_alert_info("Starting Accuracy: ID {i}...")

                ret <- modeltime_accuracy(x)

                # cli::cli_alert_success("Finished Modeltime Table: ID {id}")
                # cat("\n")

            }, error = function(e) {

                cli::cli_alert_danger(str_glue("Modeltime Accuracy (Failed): ID {i}"))
                cat("\n")

                ret <- NULL

            })

            return(ret)


        })
    )

nested_accuracy %>%
    select(id, accuracy_tables) %>%
    unnest(accuracy_tables) %>%
    group_by(id) %>%
    slice_min(rmse)

# FORECASTING ----

nested_forecast <- nested_modeltime %>%
    mutate(
        forecast_tables = pmap(list(modeltime_tables, resamples, actual_data, id), .f = function(x, r, d, i) {

            tryCatch({
                # cli::cli_alert_info("Starting Accuracy: ID {i}...")

                ret <- modeltime_forecast(
                    object      = x,
                    new_data    = testing(r),
                    actual_data = d
                )

                # cli::cli_alert_success("Finished Modeltime Table: ID {id}")
                # cat("\n")

            }, error = function(e) {

                cli::cli_alert_danger(str_glue("Modeltime Forecast (Failed): ID {i}"))
                cat("\n")

                ret <- NULL

            })

            return(ret)

        })
    )

nested_forecast %>%
    select(id, forecast_tables) %>%
    unnest(forecast_tables) %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2,
        .interactive = F
    )

# REFITTING ----

nested_refit <- nested_modeltime %>%
    mutate(
        modeltime_tables = pmap(list(modeltime_tables, actual_data, id), .f = function(x, d, i) {

            tryCatch({
                cli::cli_alert_info("Starting Refit: ID {i}...")

                ret <- modeltime_refit(x, data = d)

                cli::cli_alert_success("Finished Modeltime Refit: ID {i}")
                cat("\n")

            }, error = function(e) {

                cli::cli_alert_danger(str_glue("Modeltime Refit (Failed): ID {i}"))
                cat("\n")

                ret <- NULL

            })

            return(ret)

        })
    )

nested_refit

# FUTURE FORECAST ----

nested_future_forecast <- nested_refit %>%
    mutate(
        forecast_tables = pmap(list(modeltime_tables, future_data, actual_data, id), .f = function(x, f, d, i) {

            tryCatch({
                # cli::cli_alert_info("Starting Accuracy: ID {i}...")

                ret <- modeltime_forecast(
                    object      = x,
                    new_data    = f,
                    actual_data = d
                )

                # cli::cli_alert_success("Finished Modeltime Table: ID {id}")
                # cat("\n")

            }, error = function(e) {

                cli::cli_alert_danger(str_glue("Modeltime Forecast (Failed): ID {i}"))
                cat("\n")

                ret <- NULL

            })

            return(ret)

        })
    )

nested_future_forecast %>%
    select(id, forecast_tables) %>%
    unnest(forecast_tables) %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol  = 2,
        .interactive = F
    )




