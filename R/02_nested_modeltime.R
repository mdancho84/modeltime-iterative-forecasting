


# MODELTIME NESTED FIT ----
# - Fits on training / accuracy on testing

modeltime_nested_fit <- function(nested_data, ...,
                                 metric_set = default_forecast_accuracy_metric_set(),
                                 conf_interval = 0.95,
                                 control = control_nested_fit()) {

    t1 <- Sys.time()

    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # HANDLE INPUTS ----

    nested_data <- nested_data %>%
        select(1, ".actual_data", ".future_data", ".splits")

    id_text <- names(nested_data)[[1]]

    id_expr <- sym(id_text)

    x_expr <- sym(".splits")

    d_expr <- sym(".actual_data")


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        acc_tbl   = tibble(),
        fcast_tbl = tibble(),
        error_tbl = tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(nested_data), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, d = !! d_expr, id = !! id_expr), .f = function(x, d, id) {

                tryCatch({

                    if (control$verbose) cli::cli_alert_info(str_glue("Starting Modeltime Table: ID {id}..."))

                    model_list <- list(...)

                    safe_fit <- purrr::safely(fit, otherwise = NULL, quiet = !control$verbose)

                    # Safe fitting for each workflow in model_list ----
                    .l <- model_list %>%
                        imap(.f = function (mod, i) {

                            if (!control$verbose) {
                                suppressMessages({
                                    suppressWarnings({
                                        fit_list <- safe_fit(mod, data = training(x))
                                    })
                                })
                            } else {
                                fit_list <- safe_fit(mod, data = training(x))
                            }

                            res <- fit_list %>% pluck("result")

                            err <- fit_list %>% pluck("error", 1)

                            error_tbl <- tibble(
                                !! id_text := id,
                                .model_id   = i,
                                .model_desc = get_model_description(res),
                                .error_desc = ifelse(is.null(err), NA_character_, err)
                            )

                            logging_env$error_tbl <- bind_rows(logging_env$error_tbl, error_tbl)

                            return(res)
                        })

                    # Convert to Modeltime Table -----
                    ret <- tibble::tibble(
                        .model = .l
                    ) %>%
                        tibble::rowid_to_column(var = ".model_id") %>%
                        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

                    class(ret) <- c("mdl_time_tbl", class(ret))

                    # Calibration ----
                    suppressMessages({
                        suppressWarnings({
                            ret <- ret %>%
                                modeltime_calibrate(testing(x))
                        })
                    })


                    # Accuracy ----
                    acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                        add_column(!! id_text := id, .before = 1)

                    logging_env$acc_tbl <- bind_rows(logging_env$acc_tbl, acc_tbl)

                    # Test Forecast ----
                    suppressMessages({
                        suppressWarnings({
                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = testing(x),
                                actual_data   = d,
                                conf_interval = conf_interval
                            ) %>%
                                add_column(!! id_text := id, .before = 1)

                            logging_env$fcast_tbl <- bind_rows(logging_env$fcast_tbl, fcast_tbl)
                        })
                    })

                    # Finish ----

                    if (control$verbose) cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
                    if (control$verbose) cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(str_glue("Modeltime Table (Failed): ID {id}"))
                    cli::cli_inform(e)
                    cat("\n")

                    error_tbl <- tibble(
                        !! id_text := id,
                        .model_id   = "ALL MODELS",
                        .model_desc = "ALL MODELS FAILED",
                        .error_desc = as.character(e)
                    )

                    logging_env$error_tbl <- bind_rows(logging_env$error_tbl, error_tbl)

                    ret <- NULL

                })

                if (!control$verbose) cli::cli_progress_update(.envir = logging_env)

                return(ret)
            })
        )

    if (!control$verbose) cli::cli_progress_done(.envir = logging_env)

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        capture.output() %>%
        str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% drop_na()
    attr(nested_modeltime, "accuracy_tbl")        <- logging_env$acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- logging_env$fcast_tbl
    attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- NULL
    attr(nested_modeltime, "fit_column")          <- ".splits"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `modeltime_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)

}


print.nested_mdl_time <- function(x, ...) {

    # Collect inputs
    fit_col <- attr(x, 'fit_column')
    n_models_with_errors <- attr(x, "error_tbl") %>%
        pull(.model_id) %>%
        unique() %>%
        length()

    cat("# Nested Modeltime Table\n")
    cat("  ")
    cli::cli_text(cli::col_grey("Trained on: {fit_col} | Model Errors: [{n_models_with_errors}]"))
    # cli::cli_rule()
    class(x) <- class(x)[!(class(x) %in% c("nested_mdl_time"))]
    print(x, ...)
}

# CONTROL ----

control_nested_fit <- function(verbose = FALSE,
                               allow_par = FALSE,
                               cores = -1,
                               packages = NULL) {

    ret <- modeltime:::control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_refit"
    )

    class(ret) <- c("control_nested_fit")

    return(ret)
}

control_nested_refit <- control_nested_fit

print.control_nested_fit <- function(x, ...) {
    modeltime:::pretty_print_list(x, header = "nested fit control object")
    invisible(x)
}



# NESTED TEST ACCURACY / TEST FORECAST / ERROR REPORTING ----

modeltime_nested_accuracy <- function(object) {
    attr(object, "accuracy_tbl")
}

modeltime_nested_test_forecast <- function(object) {
    attr(object, "test_forecast_tbl")
}

modeltime_nested_error_report <- function(object) {
    attr(object, "error_tbl")
}

modeltime_nested_future_forecast <- function(object) {
    attr(object, "future_forecast_tbl")
}

modeltime_nested_best_model_report <- function(object) {
    attr(object, "best_selection_tbl")
}


# NESTED SELECT BEST ----

modeltime_nested_select_best <- function(object, metric = "rmse", minimize = TRUE,
                                         filter_forecasts = TRUE) {

    # Handle inputs
    id_text <- attr(object, "id")
    id_expr <- rlang::sym(id_text)

    metric_expr <- sym(metric)

    metric_fun <- ifelse(
        minimize,
        min,
        max
    )

    # Select best from accuracy
    best_model_by_id_tbl <- object %>%

        modeltime_nested_accuracy() %>%
        group_by(!! id_expr) %>%

        filter( (!! metric_expr) == metric_fun((!! metric_expr), na.rm = TRUE)) %>%

        slice(1) %>%
        ungroup()

    attr(object, "best_selection_tbl")  <- best_model_by_id_tbl

    best_model_by_id_tbl <- best_model_by_id_tbl %>%
        select(!! id_expr, .model_id)


    # Update Modeltime Tables
    modeltime_tables_tbl <- object %>%

        select(!! id_expr, .modeltime_tables) %>%
        unnest(.modeltime_tables) %>%
        right_join(best_model_by_id_tbl, by = c(id_text, ".model_id")) %>%

        nest(.modeltime_tables = -(!! id_expr) ) %>%
        mutate(.modeltime_tables = map(.modeltime_tables, function(x) {
            class(x) <- c("mdl_time_tbl", class(x))
            x
        }))

    object$.modeltime_tables <- modeltime_tables_tbl$.modeltime_tables


    # Filter Forecasts

    if (filter_forecasts) {

        # Updated Test Forecast
        test_forecast_tbl <- object %>%
            modeltime_nested_test_forecast()

        if (!is.null(test_forecast_tbl)) {

            test_actual <- test_forecast_tbl %>%
                filter(.model_desc == "ACTUAL")

            test_forecast <- test_forecast_tbl %>%
                right_join(best_model_by_id_tbl, by = c(id_text, ".model_id"))

            test_forecast_tbl <- bind_rows(
                test_actual,
                test_forecast
            )

            attr(object, "test_forecast_tbl")  <- test_forecast_tbl

        }

        # Updated Test Forecast
        future_forecast_tbl <- object %>%
            modeltime_nested_future_forecast()

        if (!is.null(future_forecast_tbl)) {

            future_actual <- future_forecast_tbl %>%
                filter(.model_desc == "ACTUAL")

            future_forecast <- future_forecast_tbl %>%
                right_join(best_model_by_id_tbl, by = c(id_text, ".model_id"))

            future_forecast_tbl <- bind_rows(
                future_actual,
                future_forecast
            )

            attr(object, "future_forecast_tbl")  <- future_forecast_tbl

        }

    }

    return(object)

}


# NESTED REFIT ----

modeltime_nested_refit <- function(object, control = control_nested_refit()) {

    t1 <- Sys.time()

    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # HANDLE INPUTS ----

    object <- object %>%
        select(1, ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    id_text <- names(object)[[1]]

    id_expr <- sym(id_text)

    x_expr <- sym(".modeltime_tables")

    d_expr <- sym(".actual_data")

    f_expr <- sym(".future_data")


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        fcast_tbl = tibble(),
        error_tbl = tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(object), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- object %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, d = !! d_expr, f = !! f_expr, id = !! id_expr), .f = function(x, d, f, id) {

                # Save current model descriptions
                model_desc_user_vec          <- x$.model_desc
                model_desc_modeltime_old_vec <- x$.model %>% purrr::map_chr(get_model_description)



                tryCatch({

                    if (control$verbose) cli::cli_alert_info(str_glue("Starting Modeltime Table: ID {id}..."))

                    model_list <- x$.model

                    safe_fit <- purrr::safely(fit, otherwise = NULL, quiet = !control$verbose)

                    # Safe fitting for each workflow in model_list ----
                    .l <- model_list %>%
                        imap(.f = function (mod, i) {

                            if (!control$verbose) {
                                suppressMessages({
                                    suppressWarnings({
                                        fit_list <- safe_fit(mod, data = d)
                                    })
                                })
                            } else {
                                fit_list <- safe_fit(mod, data = training(x))
                            }

                            res <- fit_list %>% pluck("result")

                            err <- fit_list %>% pluck("error", 1)

                            error_tbl <- tibble(
                                !! id_text := id,
                                .model_id   = i,
                                .model_desc = get_model_description(res),
                                .error_desc = ifelse(is.null(err), NA_character_, err)
                            )

                            logging_env$error_tbl <- bind_rows(logging_env$error_tbl, error_tbl)

                            return(res)
                        })

                    # Convert to Modeltime Table -----
                    ret <- tibble::tibble(
                        .model = .l
                    ) %>%
                        tibble::rowid_to_column(var = ".model_id") %>%
                        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

                    class(ret) <- c("mdl_time_tbl", class(ret))

                    # Update Model Descriptions
                    ret <- ret %>%
                        dplyr::mutate(.model_desc_user = model_desc_user_vec) %>%
                        dplyr::mutate(.model_desc_old  = model_desc_modeltime_old_vec) %>%
                        dplyr::mutate(.model_desc_new  = purrr::map_chr(.model, .f = get_model_description)) %>%

                        # Description Logic
                        dplyr::mutate(.model_desc = ifelse(
                            .model_desc_old == .model_desc_new,
                            # TRUE - Let User Choice Alone
                            .model_desc_user,
                            # FALSE - Model Algorithm Parameters Have Changed
                            # - Reflect Updated Model Params in Description
                            paste0("UPDATE: ", .model_desc_new)
                            )
                        ) %>%

                        # Clean up columns
                        dplyr::select(-.model_desc_user, -.model_desc_old, -.model_desc_new)

                    # Future Forecast ----
                    suppressMessages({
                        suppressWarnings({
                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = f,
                                actual_data   = d,
                                conf_interval = conf_interval
                            ) %>%
                                add_column(!! id_text := id, .before = 1)

                            logging_env$fcast_tbl <- bind_rows(logging_env$fcast_tbl, fcast_tbl)
                        })
                    })

                    # Finish ----

                    if (control$verbose) cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
                    if (control$verbose) cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(str_glue("Modeltime Table (Failed): ID {id}"))
                    cli::cli_inform(e)
                    cat("\n")

                    error_tbl <- tibble(
                        !! id_text := id,
                        .model_id   = "ALL MODELS",
                        .model_desc = "ALL MODELS FAILED",
                        .error_desc = as.character(e)
                    )

                    logging_env$error_tbl <- bind_rows(logging_env$error_tbl, error_tbl)

                    ret <- NULL

                })

                if (!control$verbose) cli::cli_progress_update(.envir = logging_env)

                return(ret)
            })
        )

    if (!control$verbose) cli::cli_progress_done(.envir = logging_env)

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        capture.output() %>%
        str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    # attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% drop_na()
    # attr(nested_modeltime, "accuracy_tbl")        <- logging_env$acc_tbl
    # attr(nested_modeltime, "test_forecast_tbl")   <- logging_env$fcast_tbl
    # attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- logging_env$fcast_tbl
    attr(nested_modeltime, "fit_column")          <- ".actual_data"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `modeltime_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)

}


