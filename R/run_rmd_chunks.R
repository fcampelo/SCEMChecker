#' Run an Rmd and capture outputs
#'
#' @param path path to Rmd file
#' @param remove_illegal_installs logical, should calls to install.packages() be
#' removed outside the "Setup" chunk?
#'
#'  @returns a list object with:
#'  \itemize{
#'     \item env: environment with variables created
#'     \item chunks: named list per chunk with fields: chunk, present,
#'           error, plot (recordedplot or NULL), plot_generated (logical),
#'           illegal_install (logical)
#'  }
#'
#'  @export

run_rmd_chunks <- function(path, remove_illegal_installs = TRUE) {

  # Parse file in path
  parsed <- parse_rmd_chunks(path)

  # detect and remove illegal installs (if needed)
  illegal_installs <- character(0)
  if (remove_illegal_installs) {
    for (chunk_name in names(parsed)) {
      if (chunk_name != "Setup") {
        code <- parsed[[chunk_name]]

        if (any(grepl("install\\.packages\\s*\\(", code))) {
          illegal_installs <- c(illegal_installs, chunk_name)
          parsed[[chunk_name]] <- gsub("install\\.packages\\s*\\([^)]*\\)",
                                       "# install.packages() removed by checker",
                                       parsed[[chunk_name]])
        }
      }
    }
  }

  # Generate new environment for running the chunks
  env <- new.env(parent = globalenv())
  chunk_results <- list()

  for (chunk_name in names(parsed)) {
    res <- list(
      chunk           = chunk_name,
      present         = TRUE,
      illegal_install = chunk_name %in% illegal_installs,
      error           = NULL,
      plot            = NULL,
      plot_generated  = FALSE)

    code <- parsed[[chunk_name]]
    expr <- try(parse(text = code), silent = TRUE)

    if (inherits(expr, "try-error")) {
      res$error <- paste0("Parse error: ", attr(expr, "condition")$message)
      chunk_results[[chunk_name]] <- res
      next
    }

    # execute silently, swallow console output, warnings/messages, and plots (use pdf(NULL))
    exec <- try({
      suppressMessages(
        suppressWarnings(
          capture.output({
            # open a null device to swallow plots
            pdf(NULL)
            on.exit(dev.off(), add = TRUE)

            # Evaluate parsed code in env
            eval(expr, envir = env)

            # try to record plot (could be empty)
            rp <- try(recordPlot(), silent = TRUE)

            if (!inherits(rp, "try-error") && !is.null(rp)) {

              # check if rp carries content
              rp_list <- try(unclass(rp), silent = TRUE)
              has_content <- FALSE

              if (!inherits(rp_list, "try-error")) {
                # typical rp unclassed is a list; check length
                has_content <- length(rp_list) > 0
              } else {
                # fallback: assume a recorded plot exists
                has_content <- TRUE
              }

              if (has_content) {
                res$plot <- rp
                res$plot_generated <- TRUE
              }
            }
          }, file = NULL)
        ))
    }, silent = TRUE)

    if (inherits(exec, "try-error")) {
      res$error <- paste0("Execution error: ", attr(exec, "condition")$message)
    }

    chunk_results[[chunk_name]] <- res
  }

  list(env = env, chunks = chunk_results)
}
