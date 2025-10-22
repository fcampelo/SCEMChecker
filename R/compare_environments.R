# compare_environments: uses template_info to focus on required variables/plots
compare_environments <- function(sub_run, ref_run, template_info) {
  results <- list()
  # iterate template chunks (so ordering / presence is respected)
  for (chunk_name in names(template_info)) {
    tpl <- template_info[[chunk_name]]
    res <- list(chunk = chunk_name, present = FALSE,
                missing_vars = NULL, type_mismatch = NULL,
                value_mismatch = NULL, missing_plot = FALSE,
                plot_present_submission = FALSE, plot_present_reference = FALSE)

    # determine presence
    res$present <- chunk_name %in% names(sub_run$chunks)

    if (!res$present) {
      results[[chunk_name]] <- res
      next
    }

    # extract submission chunk item and reference chunk item
    sub_chunk <- sub_run$chunks[[chunk_name]]
    ref_chunk <- if (chunk_name %in% names(ref_run$chunks)) ref_run$chunks[[chunk_name]] else NULL

    # plot checks
    res$plot_present_submission <- isTRUE(sub_chunk$plot_generated)
    res$plot_present_reference  <- !is.null(ref_chunk) && isTRUE(ref_chunk$plot_generated)
    if (!is.null(tpl$plot) && !res$plot_present_submission) {
      res$missing_plot <- TRUE
    }

    # variable checks: for each required variable
    for (v in names(tpl$vars)) {
      # existence in submission env?
      if (!exists(v, envir = sub_run$env, inherits = FALSE)) {
        res$missing_vars <- c(res$missing_vars, v)
        next
      }
      # existence in reference env?
      if (is.null(ref_run) || !exists(v, envir = ref_run$env, inherits = FALSE)) {
        # we'll still compare if reference missing
        res$value_mismatch <- c(res$value_mismatch,
                                paste0(v, ": reference missing"))
        next
      }
      # type check
      sub_obj <- get(v, envir = sub_run$env)
      ref_obj <- get(v, envir = ref_run$env)
      sub_classes <- class(sub_obj)
      req_class <- tpl$vars[[v]]
      # if required class not in submission class vector -> type mismatch
      if (!(req_class %in% sub_classes)) {
        res$type_mismatch <- rbind(
          res$type_mismatch,
          data.frame(variable = v, expected = req_class, actual = paste(sub_classes, collapse = "/"), stringsAsFactors = FALSE)
        )
      }
      # value check
      cmp <- compare_values(sub_obj, ref_obj)
      if (!cmp$match) {
        res$value_mismatch <- c(res$value_mismatch, paste0(v, ": ", cmp$detail))
      }
    } # end vars

    results[[chunk_name]] <- res
  } # end chunks
  results
}
