
# NOTES ----
# * What about parsnip models? ----
# * What about workflowsets? See modeltime_fit_workflowset() ----
# * Will the Nested Modeltime Table get too big? https://diskframe.com/index.html ----

# MODELTIME NESTED FIT ----
# - Fits on training / accuracy on testing

modeltime_nested_fit <- function(nested_data, ...,
                                 calibrate = TRUE,
                                 control = control_nested_fit()) {

    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # HANDLE INPUTS ----

    id_text <- names(nested_data)[[1]]

    id_expr <- sym(id_text)

    x_expr <- sym(".splits")



    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        acc_tbl   = tibble(),
        error_tbl = tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(nested_data), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        mutate(
            .modeltime_tables = pmap(.l = list(x = !! x_expr, id = !! id_expr), .f = function(x, id) {

                tryCatch({



                    if (control$verbose) cli::cli_alert_info(str_glue("Starting Modeltime Table: ID {id}..."))

                    model_list <- list(...)

                    safe_fit <- purrr::safely(fit, otherwise = NULL, quiet = !control$verbose)

                    # Safe fitting for each workflow in model_list ----
                    .l <- model_list %>%
                        imap(.f = function (mod, i) {

                            if (!control$verbose) {
                                suppressMessages({
                                    fit_list <- safe_fit(mod, data = training(x))
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

                    # Calibrate & Accuracy ----
                    if (calibrate) {

                        suppressWarnings({
                            ret <- ret %>%
                                modeltime_calibrate(testing(x))
                        })

                        acc_tbl <- modeltime_accuracy(ret) %>%
                            add_column(!! id_text := id, .before = 1)

                        logging_env$acc_tbl <- bind_rows(logging_env$acc_tbl, acc_tbl)
                    }


                    if (control$verbose) cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
                    if (control$verbose) cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(str_glue("Modeltime Table (Failed): ID {id}"))
                    cli::cli_inform(e)
                    cat("\n")

                    error_tbl <- tibble(
                        !! id_text := id,
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

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    attr(nested_modeltime, "id")           <- id_text
    attr(nested_modeltime, "error_tbl")    <- logging_env$error_tbl %>% drop_na()
    attr(nested_modeltime, "accuracy_tbl") <- logging_env$acc_tbl



    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Run `modeltime_nested_error_report(object)` to review errors.")
    }



    return(nested_modeltime)

}


print.nested_mdl_time <- function(x, ...) {
    cat("# Nested Modeltime Table\n")
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

print.control_nested_fit <- function(x, ...) {
    pretty_print_list(x, header = "nested fit control object")
    invisible(x)
}

pretty_print_list <- function(x, header=NULL, justify="left", sep=":") {

    if (!is.list(x) || is.null(names(x)))
        stop("x must be a list containing named objects")
    if (!is.null(header) && (!is.character(header) || length(header) > 1))
        stop("header must be a single character string")
    if (!is.character(justify) || length(justify) > 1)
        stop("justify must be a single character string")
    if (!is.character(sep) || length(sep) > 1)
        stop("sep must be a single character string")

    justify <- match.arg(justify, c("none","left","right","decimal"))

    if (!is.null(header))
        cat(header,"\n", rep("-",nchar(header)),"\n",sep="")

    # prune list of NULL values.
    # if x <- list("some really large name"=NULL, cat="dog")
    # the spearator will be spaced way too far to the right
    # due to the influence of the first entry name. thus,
    # we eliminate any such NULL entries altogether to
    # avoid this problem
    x <- x[!unlist(lapply(x, is.null))]

    if (!length(x))
        return(invisible(NULL))

    categories <- format(names(x), justify=justify)

    # Cat Print
    for (i in seq(along=categories)){
        if (!is.null(x[[i]]))
            cat(categories[i], sep, x[[i]], "\n", sep=" ")
    }

    invisible(NULL)
}



# NESTED ACCURACY ----

modeltime_nested_accuracy <- function(object) {
    attr(object, "accuracy_tbl")
}

modeltime_nested_error_report <- function(object) {
    attr(object, "error_tbl")
}

# NESTED FORECAST ----

# function(object) {
#
#     object %>%
#         mutate(
#             forecast_tables = pmap(list(.modeltime_tables, .splits, .actual_data, ), .f = function(x, r, d, i) {
#
#                 tryCatch({
#                     # cli::cli_alert_info(str_glue("Starting Accuracy: ID {i}..."))
#
#                     ret <- modeltime_forecast(
#                         object      = x,
#                         new_data    = testing(r),
#                         actual_data = d
#                     )
#
#                     # cli::cli_alert_success(str_glue("Finished Modeltime Table: ID {id}"))
#                     # cat("\n")
#
#                 }, error = function(e) {
#
#                     cli::cli_alert_danger(str_glue("Modeltime Forecast (Failed): ID {i}"))
#                     cat("\n")
#
#                     ret <- NULL
#
#                 })
#
#                 return(ret)
#
#             })
#         )
#
# }
#
