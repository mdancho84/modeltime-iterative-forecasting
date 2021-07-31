
# NOTES ----
# What about parsnip models? ----
# What about workflowsets? See modeltime_fit_workflowset() ----
# Will the Nested Modeltime Table get too big? https://diskframe.com/index.html ----

# MODELTIME NESTED FIT ----

modeltime_nested_fit <- function(nested_data, ...,
                                 fit_type = c("train_test", "actual"),
                                 calibrate = TRUE,
                                 control = NULL) {

    # CHECKS ----
    # - Nested Data Structure

    # HANDLE INPUTS ----

    id_text <- names(nested_data)[[1]]

    id_expr <- sym(id_text)

    fit_type <- tolower(fit_type[[1]])

    if (fit_type == "actual") {
        x_expr <- sym(".actual_data")
    } else {
        x_expr <- sym(".splits")
    }

    # ERRORS ----
    err_env <- rlang::env(
        error_list = list()
    )

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, id = !! id_expr), .f = function(x, id) {

                tryCatch({
                    cli::cli_alert_info(str_glue("Starting Modeltime Table: ID {id}..."))

                    model_list <- list(...)

                    if (fit_type != "actual") {

                        ret <- model_list %>%
                            imap(.f = function (mod, i) {
                                # Use this to test a model failure:
                                if (i == 1 && id == "1_1") stop("Model failed")
                                fit(mod, data = training(x))
                            }) %>%
                            as_modeltime_table()

                        if (calibrate) {
                            ret <- ret %>%
                                modeltime_calibrate(testing(x))
                        }

                    } else {
                        ret <- model_list %>%
                            imap(.f = function (mod, i) {
                                fit(mod, data = x)
                            }) %>%
                            as_modeltime_table()
                    }

                    cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
                    cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(str_glue("Modeltime Table (Failed): ID {id}"))
                    cli::cli_inform(e)
                    cat("\n")

                    error_tbl <- tibble(
                        !! id_text := id,
                        error_code = as.character(e)
                    )

                    err_env$error_list <- list(err_env$error_list, error_tbl)

                    ret <- NULL

                })

                return(ret)
            })
        )

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    # error_tbl <- bind_rows(error_list)

    attr(nested_modeltime, "id") <- id_text
    attr(nested_modeltime, "error_tbl") <- err_env$error_list %>% bind_rows()

    return(nested_modeltime)

}


print.nested_mdl_time <- function(x, ...) {
    cat("# Nested Modeltime Table\n")
    class(x) <- class(x)[!(class(x) %in% c("nested_mdl_time"))]
    print(x, ...)
}


# NESTED ACCURACY ----

modeltime_nested_accuracy <- function(object) {

    id_text <- names(object)[[1]]

    id_expr <- sym(id_text)

    modeltime_table_expr <- sym(".modeltime_tables")

    ret <- object %>%
        mutate(
            .accuracy_tables = pmap(list(!! modeltime_table_expr, !! id_expr), .f = function(x, i) {

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

    ret %>%
        select(!! id_expr, !! sym(".accuracy_tables")) %>%
        unnest(!! sym(".accuracy_tables"))

}


# NESTED FORECAST ----

function(object) {

    object %>%
        mutate(
            forecast_tables = pmap(list(.modeltime_tables, .splits, .actual_data, ), .f = function(x, r, d, i) {

                tryCatch({
                    # cli::cli_alert_info(str_glue("Starting Accuracy: ID {i}..."))

                    ret <- modeltime_forecast(
                        object      = x,
                        new_data    = testing(r),
                        actual_data = d
                    )

                    # cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
                    # cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(str_glue("Modeltime Forecast (Failed): ID {i}"))
                    cat("\n")

                    ret <- NULL

                })

                return(ret)

            })
        )

}

