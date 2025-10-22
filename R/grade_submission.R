# =================== (5) top-level: grade_submission ===================

# - template_path: template Rmd path
# - submission_path: student's Rmd
# - reference_path: reference Rmd
# - out_dir: directory where intermediate results, RDS, Rmd and html will be written
# - render_report: whether to call rmarkdown::render() on feedback Rmd
grade_submission <- function(template_path,
                             submission_path,
                             reference_path,
                             out_dir = tempdir(),
                             reqvars_str = "Required variables",
                             reqplot_str = "Required plot",
                             render_report = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # parse template info
  template_chunks <- parse_rmd_chunks(template_path)
  template_info <- lapply(template_chunks, parse_requirements,
                          reqvars_str = reqvars_str, reqplot_str = reqplot_str)

  # run submission
  sub_run <- run_rmd_chunks(submission_path, remove_illegal_installs = TRUE)

  # attempt to get MY_STUDENT_ID from submission env
  student_id <- if (exists("MY_STUDENT_ID", envir = sub_run$env, inherits = FALSE)) {
    get("MY_STUDENT_ID", envir = sub_run$env)
  } else {
    NA
  }

  # run reference with student id assigned
  ref_env <- new.env(parent = globalenv())
  if (!is.na(student_id)) assign("MY_STUDENT_ID", student_id, envir = ref_env)

  # We'll run reference by parsing its chunks and evaluating them in ref_env
  # but reusing run_rmd_chunks would create a fresh env; instead we run run_rmd_chunks then copy env?
  # Simpler: run_rmd_chunks and then, if student id was assigned before running, it will be used.
  # So we must run run_rmd_chunks after setting MY_STUDENT_ID in a new env -- run_rmd_chunks always creates its own env,
  # so we need a variant; easiest: temporarily create a file copy of reference and evaluate in an env with MY_STUDENT_ID set.
  # But for simplicity we will mimic run_rmd_chunks but with env we control:

  # parse reference chunks
  ref_parsed <- parse_rmd_chunks(reference_path)
  # remove illegal installs from reference? not needed but safe
  for (cn in names(ref_parsed)) {
    ref_parsed[[cn]] <- gsub("install\\.packages\\s*\\([^)]*\\)",
                             "# install.packages() removed by checker",
                             ref_parsed[[cn]])
  }

  # execute ref chunks in ref_env, silently, capturing plots
  ref_chunks_results <- list()
  for (chunk_name in names(ref_parsed)) {
    res <- list(chunk = chunk_name, present = TRUE, illegal_install = FALSE,
                error = NULL, plot = NULL, plot_generated = FALSE)
    code <- ref_parsed[[chunk_name]]
    expr <- try(parse(text = code), silent = TRUE)
    if (inherits(expr, "try-error")) {
      res$error <- paste0("Parse error: ", attr(expr, "condition")$message)
      ref_chunks_results[[chunk_name]] <- res
      next
    }
    exec <- try({
      suppressMessages(
        suppressWarnings(
          capture.output({
            pdf(NULL)
            on.exit(dev.off(), add = TRUE)
            eval(expr, envir = ref_env)
            rp <- try(recordPlot(), silent = TRUE)
            if (!inherits(rp, "try-error") && !is.null(rp)) {
              rp_list <- try(unclass(rp), silent = TRUE)
              has_content <- FALSE
              if (!inherits(rp_list, "try-error")) {
                has_content <- length(rp_list) > 0
              } else {
                has_content <- TRUE
              }
              if (has_content) {
                res$plot <- rp
                res$plot_generated <- TRUE
              }
            }
          }, file = NULL)
        )
      )
    }, silent = TRUE)
    if (inherits(exec, "try-error")) {
      res$error <- paste0("Execution error: ", attr(exec, "condition")$message)
    }
    ref_chunks_results[[chunk_name]] <- res
  }

  ref_run <- list(env = ref_env, chunks = ref_chunks_results)

  # Now compare
  compare_res <- compare_environments(sub_run, ref_run, template_info)

  # Build summary_df similar to your previous summary layout
  results_list <- list()
  for (chunk_name in names(template_info)) {
    tpl <- template_info[[chunk_name]]
    sub_chunk <- if (chunk_name %in% names(sub_run$chunks)) sub_run$chunks[[chunk_name]] else NULL
    # compose flattened info for summary row
    present <- chunk_name %in% names(sub_run$chunks)
    illegal_install <- if (present) isTRUE(sub_run$chunks[[chunk_name]]$illegal_install) else FALSE
    err <- if (present && !is.null(sub_chunk$error)) sub_chunk$error else ""
    comp <- compare_res[[chunk_name]]
    missing_vars <- if (!is.null(comp$missing_vars)) paste(comp$missing_vars, collapse = ", ") else ""
    type_mismatch <- if (!is.null(comp$type_mismatch)) {
      apply(comp$type_mismatch, 1, function(r) paste0(r["variable"], ": (", r["expected"], " -> ", r["actual"], ")")) %>% paste(collapse = "; ")
    } else ""
    missing_plot <- isTRUE(comp$missing_plot)

    results_list[[chunk_name]] <- tibble::tibble(
      chunk = chunk_name,
      present = present,
      illegal_install = illegal_install,
      error = ifelse(is.null(err), "", err),
      missing_vars = missing_vars,
      type_mismatch = type_mismatch,
      missing_plot = missing_plot
    )
  }

  summary_df <- dplyr::bind_rows(results_list)

  # Prepare compact "values" objects to save for the feedback Rmd:
  # For each chunk, save required variable values (as a named list) for submission and reference
  submission_values <- list()
  reference_values  <- list()
  # Save also chunk-level info objects for replayPlot usage
  submission_chunks_export <- list()
  reference_chunks_export  <- list()

  for (chunk_name in names(template_info)) {
    tpl <- template_info[[chunk_name]]
    # submission side
    sv <- list(vars = list())
    if (chunk_name %in% names(sub_run$chunks)) {
      for (v in names(tpl$vars)) {
        if (exists(v, envir = sub_run$env, inherits = FALSE)) {
          sv$vars[[v]] <- get(v, envir = sub_run$env)
        } else sv$vars[[v]] <- NULL
      }
      submission_chunks_export[[chunk_name]] <- list(plot = sub_run$chunks[[chunk_name]]$plot)
    } else {
      submission_chunks_export[[chunk_name]] <- list(plot = NULL)
    }
    submission_values[[chunk_name]] <- sv

    # reference side
    rv <- list(vars = list())
    if (chunk_name %in% names(ref_run$chunks)) {
      for (v in names(tpl$vars)) {
        if (exists(v, envir = ref_run$env, inherits = FALSE)) {
          rv$vars[[v]] <- get(v, envir = ref_run$env)
        } else rv$vars[[v]] <- NULL
      }
      reference_chunks_export[[chunk_name]] <- list(plot = ref_run$chunks[[chunk_name]]$plot)
    } else {
      reference_chunks_export[[chunk_name]] <- list(plot = NULL)
    }
    reference_values[[chunk_name]] <- rv
  }

  # Save RDS files to out_dir (these will be loaded by the feedback Rmd)
  submission_values_file <- file.path(out_dir, "submission_values.rds")
  reference_values_file  <- file.path(out_dir, "reference_values.rds")
  submission_chunks_file <- file.path(out_dir, "submission_chunks.rds")
  reference_chunks_file  <- file.path(out_dir, "reference_chunks.rds")
  summary_df_file        <- file.path(out_dir, "summary_df.rds")

  saveRDS(submission_values, submission_values_file)
  saveRDS(reference_values, reference_values_file)
  saveRDS(submission_chunks_export, submission_chunks_file)
  saveRDS(reference_chunks_export, reference_chunks_file)
  saveRDS(summary_df, summary_df_file)

  # Generate feedback Rmd (which will read these RDS files)
  feedback_rmd <- file.path(out_dir, "feedback_report.Rmd")
  generate_feedback_rmd(summary_df = summary_df,
                        compare_results = compare_res,
                        submission_values_file = submission_values_file,
                        reference_values_file  = reference_values_file,
                        submission_chunks_file = submission_chunks_file,
                        reference_chunks_file  = reference_chunks_file,
                        output_rmd = feedback_rmd)

  # Also save the summary_df for convenience
  # (already saved as summary_df_file)

  # Optionally render to HTML
  output_html <- NULL
  if (render_report) {
    # copy necessary RDS files to out_dir working dir (they already are there)
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(out_dir)
    output_html <- tryCatch({
      rmarkdown::render(basename(feedback_rmd), quiet = TRUE)
    }, error = function(e) {
      warning("Rendering feedback Rmd failed: ", e$message)
      NULL
    })
  }

  return(list(
    student_id = student_id,
    summary_df = summary_df,
    submission_run = sub_run,
    reference_run = ref_run,
    compare_results = compare_res,
    feedback_rmd = feedback_rmd,
    feedback_html = output_html,
    rds_files = list(submission_values = submission_values_file,
                     reference_values = reference_values_file,
                     submission_chunks = submission_chunks_file,
                     reference_chunks = reference_chunks_file,
                     summary_df = summary_df_file)
  ))
}

# =================== End of script ===================

# Example usage:
# result <- grade_submission("Template_solution.Rmd",
#                            "Student_submission.Rmd",
#                            "Reference_answers.Rmd",
#                            out_dir = "grading_output",
#                            render_report = TRUE)
#
# result$summary_df  # per-chunk summary
# result$feedback_rmd  # path to generated Rmd
# result$feedback_html # path to rendered html (if render_report = TRUE)
