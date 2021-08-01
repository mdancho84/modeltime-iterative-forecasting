
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
    table_env <- rlang::env(
        acc_tbl   = tibble(),
        error_tbl = tibble()

    )

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, id = !! id_expr), .f = function(x, id) {

                tryCatch({
                    cli::cli_alert_info(str_glue("Starting Modeltime Table: ID {id}..."))

                    model_list <- list(...)

                    safe_fit <- purrr::safely(fit, otherwise = NULL, quiet = FALSE)

                    if (fit_type != "actual") {

                        .l <- model_list %>%
                            imap(.f = function (mod, i) {
                                # Use this to simulate a model failure:
                                # if (i == 1 && id == "1_1") stop("Model failed")
                                safe_fit(mod, data = training(x)) %>% pluck("result")
                            })

                        ret <- tibble::tibble(
                            .model = .l
                        ) %>%
                            tibble::rowid_to_column(var = ".model_id") %>%
                            dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

                        class(ret) <- c("mdl_time_tbl", class(ret))

                        if (calibrate) {
                            ret <- ret %>%
                                modeltime_calibrate(testing(x))

                            acc_tbl <- modeltime_accuracy(ret) %>%
                                add_column(!! id_text := id, .before = 1)

                            table_env$acc_tbl <- bind_rows(table_env$acc_tbl, acc_tbl)
                        }

                    } else {

                        .l <- model_list %>%
                            imap(.f = function (mod, i) {
                                safe_fit(mod, data = x) %>% pluck("result")
                            })

                        ret <- tibble::tibble(
                            .model = .l
                        ) %>%
                            tibble::rowid_to_column(var = ".model_id") %>%
                            dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

                        class(ret) <- c("mdl_time_tbl", class(ret))
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

                    table_env$error_tbl <- bind_rows(table_env$error_tbl, error_tbl)

                    ret <- NULL

                })

                return(ret)
            })
        )

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    attr(nested_modeltime, "id")           <- id_text
    attr(nested_modeltime, "error_tbl")    <- table_env$error_tbl
    attr(nested_modeltime, "accuracy_tbl") <- table_env$acc_tbl

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

