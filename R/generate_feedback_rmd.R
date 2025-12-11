# =================== (4) generate_feedback_rmd ===================

# Writes an Rmd feedback file which loads previously saved RDS files:
# - submission_results_file (contains list: env not saved, but we save chunk-level info and actual variable values)
# - reference_results_file
#
# We will save smaller R objects to disk so the Rmd can load them and display content
generate_feedback_rmd <- function(summary_df,
                                  compare_results,
                                  submission_values_file,
                                  reference_values_file,
                                  submission_chunks_file,
                                  reference_chunks_file,
                                  output_rmd) {

  lines <- c(
    "---",
    "title: \"Feedback Report\"",
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "submission_values <- readRDS('submission_values.rds')",
    "reference_values  <- readRDS('reference_values.rds')",
    "submission_chunks  <- readRDS('submission_chunks.rds')",
    "reference_chunks   <- readRDS('reference_chunks.rds')",
    "summary_df <- readRDS('summary_df.rds')",
    "```",
    ""
  )

  # Add a high-level summary table
  # lines <- c(lines,
  #            "## Summary",
  #            "",
  #            "```{r summary_table, echo=FALSE}",
  #            "knitr::kable(summary_df)",
  #            "```",
  #            "")

  # For each chunk, create a section showing submission & reference
  for (i in seq_len(nrow(summary_df))) {
    chunk_name <- summary_df$chunk[i]
    lines <- c(lines,
               paste0("## Chunk: ", chunk_name),
               "")
    # Errors / issues
    lines <- c(lines,
               "### Issues / Checks",
               "",
               paste0("- Present in submission: `", summary_df$present[i], "`"),
               paste0("- Illegal install.packages detected: `", summary_df$illegal_install[i], "`"),
               "")
    # error text
    if (nzchar(summary_df$error[i])) {
      lines <- c(lines, paste0("- Execution/Error: `", summary_df$error[i], "`"),
                 "")
    }
    if (nzchar(summary_df$missing_vars[i])) {
      lines <- c(lines, paste0("- Missing variables: `", summary_df$missing_vars[i], "`"),
                 "")
    }
    if (nzchar(summary_df$type_mismatch[i])) {
      lines <- c(lines, paste0("- Type mismatches: `", summary_df$type_mismatch[i], "`"),
                 "")
    }
    # if (isTRUE(summary_df$missing_plot[i])) {
    #   lines <- c(lines, "- Missing expected plot")
    # }
    # lines <- c(lines, "")

    # Show required variables/types and submission variables/types
    #lines <- c(lines, "### Submission output\n\n")
    # lines <- c(lines,
    #            "",
    #            paste0("```{r sub_vars", i, ", echo=FALSE, results='asis'}"),
    #            paste0("sv <- submission_values[['", chunk_name, "']]"),
    #            paste0("rv <- reference_values[['", chunk_name, "']]"),
    #            "if (is.null(sv)) cat('No outputs captured for this chunk in submission.') else {",
    #            "  if (length(sv$vars)>0) {",
    #            "    for (vn in names(sv$vars)) {",
    #            "      cat('\\n\\nSubmitted ', vn, ': ', class(sv$vars[[vn]]),",
    #            "        '\\n\\nReference ', vn, ': ', class(rv$vars[[vn]]))",
    #            "    }",
    #            "  } else cat('No required variables captured.\\n')",
    #            "}",
    #            "```",
    #            "")



    # Show submission variables and values (only the required variables; load from submission_values)
    # lines <- c(lines, "### Submission output", "")
    # lines <- c(lines,
    #            paste0("```{r sub_vars", i, ", echo=FALSE, results='asis'}"),
    #            paste0("sv <- submission_values[['", chunk_name, "']]"),
    #            "if (is.null(sv)) cat('No outputs captured for this chunk in submission.\\n') else {",
    #            "  if (length(sv$vars)>0) {",
    #            "    for (vn in names(sv$vars)) {",
    #            "      cat('**', vn, '**: ', '\\n')",
    #            "      print(sv$vars[[vn]])",
    #            "      cat('\\n---\\n')",
    #            "    }",
    #            "  } else cat('No required variables captured.\\n')",
    #            "}",
    #            "```",
    #            "")

    # Plot (submission)
    # lines <- c(lines,
    #            "```{r sub_plot, echo=FALSE, fig.width=4, fig.height=4}",
    #            paste0("sp <- submission_chunks[['", chunk_name, "']]$plot"),
    #            "if (!is.null(sp)) replayPlot(sp) else cat('No plot in submission for this chunk.\\n')",
    #            "```",
    #            "")

    # Reference output
    # lines <- c(lines, "### Reference output", "")
    # lines <- c(lines,
    #            paste0("```{r ref_vars", i, ", echo=FALSE, results='asis'}"),
    #            paste0("rv <- reference_values[['", chunk_name, "']]"),
    #            "if (is.null(rv)) cat('No outputs captured for this chunk in reference.\\n') else {",
    #            "  if (length(rv$vars)>0) {",
    #            "    for (vn in names(rv$vars)) {",
    #            "      cat('**', vn, '**: ', '\\n')",
    #            "      print(rv$vars[[vn]])",
    #            "      cat('\\n---\\n')",
    #            "    }",
    #            "  } else cat('No required variables captured.\\n')",
    #            "}",
    #            "```",
    #            "")
    # Reference plot
    # lines <- c(lines,
    #            "```{r ref_plot, echo=FALSE, fig.width=4, fig.height=4}",
    #            paste0("rp <- reference_chunks[['", chunk_name, "']]$plot"),
    #            "if (!is.null(rp)) replayPlot(rp) else cat('No reference plot for this chunk.\\n')",
    #            "```",
    #            "")
    # Add newline
    lines <- c(lines, "")
  }

  # Write to file
  writeLines(lines, con = output_rmd)
}
