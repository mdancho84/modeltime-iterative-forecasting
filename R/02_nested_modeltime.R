
# NOTES ----
# What about parsnip models? ----
# What about workflowsets? See modeltime_fit_workflowset() ----
# Will the Nested Modeltime Table get too big? https://diskframe.com/index.html ----

# MODELTIME NESTED FIT ----

modeltime_nested_fit <- function(nested_data, ..., calibrate = TRUE) {

    .id_var <- names(nested_data)[[1]]

    id_expr <- enquo(.id_var)

    if (!calibrate) {
        x_expr <- sym(".actual_data")
    } else {
        x_expr <- sym(".splits")
    }

    nested_modeltime <- nested_data %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, id = !! id_expr), .f = function(x, id) {

                tryCatch({
                    cli::cli_alert_info("Starting Modeltime Table: ID {id}...")

                    model_list <- list(...)

                    if (calibrate) {
                        ret <- model_list %>%
                            imap(.f = function (mod, i) {
                                fit(mod, data = training(x))
                            }) %>%
                            as_modeltime_table() %>%
                            modeltime_calibrate(testing(x))
                    } else {
                        ret <- model_list %>%
                            imap(.f = function (mod, i) {
                                fit(mod, data = x)
                            }) %>%
                            as_modeltime_table()
                    }

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

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

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


