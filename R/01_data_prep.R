


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

# TEST ERRORS:
# walmart_sales_weekly %>%
#     select(id, Date, Weekly_Sales) %>%
#     set_names(c("id", "date", "value")) %>%
#     mutate(value = NA) %>%
#     extend_timeseries(.id_var = id, .value = value, .length_out = 52)



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



split_nested_timeseries <- function(.data, .length_test, .length_train = NULL, ...) {

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


