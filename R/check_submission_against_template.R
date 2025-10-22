#' Check the consistency of an Rmd submission file against a reference template
#'
#' This function runs the initial checks of a submission file to ensure that
#' \itemize{
#'    \item all required code blocks are present
#'    \item all required variables are generated
#'    \item all required variables are of the correct type
#'    \item no code block is returning any errors
#'    \item no rogue calls to `install.packages` are done.
#' }
#'
#' @param template_path path to the template .Rmd file
#' @param submission_path path to the submission .Rmd file
#' @param reqvars_str Indicator string for definition of required variables
#' @param reqplot_str Indicator string for definition of required variables
#'
#' @returns a data frame with the summary of the checks done, and specific indications
#' of any problem found.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom grDevices pdf dev.off recordPlot
#' @importFrom utils capture.output
#'
#' @export

check_submission_against_template <- function(template_path,
                                              submission_path,
                                              reqvars_str = "Required variables",
                                              reqplot_str = "Required plot") {

  # ---------- Step 1: Parse template ----------
  template_chunks <- parse_rmd_chunks(template_path)
  template_info   <- lapply(template_chunks, parse_requirements,
                            reqvars_str = reqvars_str,
                            reqplot_str = reqplot_str)

  # ---------- Step 2: Parse submission ----------
  submission_chunks <- parse_rmd_chunks(submission_path)

  # ---------- Step 3: Check for illegal install.packages ----------
  illegal_installs <- character(0)
  for (chunk_name in names(submission_chunks)) {
    if (chunk_name != "Setup") {
      code <- submission_chunks[[chunk_name]]
      if (any(grepl("install\\.packages\\s*\\(", code))) {
        illegal_installs <- c(illegal_installs, chunk_name)
        # Remove or comment out install.packages calls
        submission_chunks[[chunk_name]] <-
          gsub("install\\.packages\\s*\\([^)]*\\)",
               "# install.packages() removed by checker",
               submission_chunks[[chunk_name]])
      }
    }
  }

  # ---------- Step 4: Validate submission ----------
  env <- new.env(parent = globalenv())
  results <- list()

  for (chunk_name in names(template_chunks)) {
    res <- list(
      chunk           = chunk_name,
      present         = chunk_name %in% names(submission_chunks),
      illegal_install = chunk_name %in% illegal_installs,
      error           = NULL,
      missing_vars    = NULL,
      missing_plot    = NULL,
      type_mismatch   = NULL)

    if (!res$present) {
      results[[chunk_name]] <- res
      next
    }

    code <- submission_chunks[[chunk_name]]
    expr <- try(parse(text = code), silent = TRUE)
    if (inherits(expr, "try-error")) {
      res$error <- paste("Parse error:", attr(expr, "condition")$message)
      results[[chunk_name]] <- res
      next
    }

    # Detect if code produces a plot
    plot_generated <- FALSE

    exec <- try(
      suppressMessages(
        suppressWarnings(
          capture.output({
            # open a null graphics device to swallow plots
            pdf(NULL)
            on.exit(dev.off(), add = TRUE)

            # eval code
            eval(expr, envir = env)

            # record anything plotted
            rp <- recordPlot()
            plot_generated <<- length(rp[[1]]) > 0  # TRUE if anything was drawn
          }, file = NULL)
        )
      ),
      silent = TRUE
    )

    if (inherits(exec, "try-error")) {
      res$error <- paste("Execution error:", attr(exec, "condition")$message)
      results[[chunk_name]] <- res
      next
    }

    expected_vars <- template_info[[chunk_name]]$vars
    if (length(expected_vars) > 0) {
      for (v in names(expected_vars)) {
        if (!exists(v, envir = env)) {
          res$missing_vars <- c(res$missing_vars, v)
        } else {
          actual_class <- class(get(v, envir = env))
          if (!(expected_vars[[v]] %in% actual_class)) {
            res$type_mismatch <- rbind(
              res$type_mismatch,
              data.frame(variable = v, expected = expected_vars[[v]], actual = actual_class)
            )
          }
        }
      }
    }

    expected_plot <- !is.null(template_info[[chunk_name]]$plot)

    if (expected_plot && !plot_generated) {
      res$missing_plot <- TRUE
    }


    results[[chunk_name]] <- res
  }

  # ---------- Step 5: Summarize ----------
  summary_df <- bind_rows(
    lapply(results,
           function(r) {
             dplyr::tibble(
               chunk         = r$chunk,
               present       = r$present,
               illegal_install = r$illegal_install,
               error         = ifelse(is.null(r$error), "", r$error),
               missing_vars  = ifelse(is.null(r$missing_vars), "",
                                      paste(r$missing_vars,
                                            collapse = ", ")),
               type_mismatch = ifelse(is.null(r$type_mismatch),
                                      "",
                                      paste(r$type_mismatch[, 1], ": (",
                                            r$type_mismatch[, 2], "-->",
                                            r$type_mismatch[, 3], ")",
                                            collapse = "; ")),
               missing_plot  = ifelse(is.null(r$missing_plot),
                                      FALSE, TRUE))}))

  class(summary_df) <- c("SCEMChecker", class(summary_df))
  return(summary_df)
}
