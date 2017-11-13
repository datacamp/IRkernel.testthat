#' Project reporter: send test results to IRkernel, for use with DataCamp Projects.
#'
#' This reporter subclasses the ListReporter, to send detailed test information for
#' use with jupyter notebooks.
#'
#' @export
ProjectReporter <- R6::R6Class("ProjectReporter", inherit = testthat::ListReporter,
  public = list(
    all_tests = NULL,
    initialize = function(...) {
      super$initialize(...)
      self$all_tests = testthat:::Stack$new()
    },
    add_result = function(context, test, result) {
      super$add_result(context, test, result)
      if (!testthat:::expectation_ok(result)) {
        testthat::skip("")
      }
    },
    end_reporter = function() {
        test_res <- lapply(self$results$as_list(),
                           self$dump_test)
        test_df <- data.frame(do.call(rbind, test_res))

        # summarize number of tests, etc..
        summary <- list(
                tests = length(test_res),
                failures = sum(test_df$outcome == 'fail'),
                errors = sum(test_df$outcome == 'error')
                )

        payload <- list(
                success = all(as.logical(test_df$success)),
                summary = summary,
                tests = test_res
                )
        # Note: may want to use repr package instead
        IRdisplay::publish_mimebundle(list(
                'application/json' = jsonlite::toJSON(payload, auto_unbox = TRUE)
                ))

        payload
    },
    dump_test = function(test) {
        message <- paste(
            lapply(test$results, `[[`, 'message'),
            collapse = '\n')

        res <- testthat:::sumarize_one_test_results(test)
        success <- !any(res$failed, res$error)
        # figure out outcome, e.g. for counting errors later
        if (!success) {
            outcome <- if (res$failed) 'fail' else 'error'
        } else outcome <- 'success'

        list(name = test$test,
             message = message,
             success = success,
             outcome = outcome)
    },
    print = function() {
      end_report <- self$end_reporter()
      summary <- end_report$summary
      tests <- end_report$tests
      cat((summary$tests - summary$failures - summary$errors), "/", summary$tests, " tests passed",sep = "")
      cat("\n")
      for(i in seq_along(tests)) {
        test <- tests[[i]]
        if(test$success) next
        cat(">", test$outcome, "::", test$name)
        cat("\n")
        cat( test$message, "\n---\n")
      }
      invisible(self)
    }
  ),
  private = list(
  )
)

#' Creates a project reporter and executes tests.
#'
#' @param test_expr testthat code to execute
#'
#' @export
run_tests <- function(test_expr) {
    # create reporter
    reporter <- ProjectReporter$new()
    reporter$start_file('some name')
    # setup and run tests
    env = testthat::test_env()
    tests <- substitute(test_expr)
    testthat::with_reporter(
        reporter = reporter, start_end_reporter = TRUE,
        eval(tests, envir = env)
        )
    reporter
}
