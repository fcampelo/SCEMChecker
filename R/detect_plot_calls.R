# Detect plotting calls in code (excluding commented lines)
detect_plot_calls <- function(code_lines) {
  # Common plot function call patterns
  plot_patterns <- c(
    "\\bplot\\s*\\(",
    "\\bbarplot\\s*\\(",
    "\\bhist\\s*\\(",
    "\\bboxplot\\s*\\(",
    "\\bpie\\s*\\(",
    "\\bqqplot\\s*\\(",
    "\\bimage\\s*\\(",
    "\\bcontour\\s*\\(",
    "\\bfilled\\.contour\\s*\\(",
    "\\bpersp\\s*\\(",
    "\\bheatmap\\s*\\(",
    "\\bmatplot\\s*\\(",
    "\\bmatpoints\\s*\\(",
    "\\bmatlines\\s*\\(",
    "\\bcurve\\s*\\(",
    "\\bsegments\\s*\\(",
    "\\barrows\\s*\\(",
    "\\bpoints\\s*\\(",
    "\\blines\\s*\\(",
    "\\btext\\s*\\(",
    "\\bggplot\\s*\\(",
    "\\bqplot\\s*\\(",
    "\\bautoplot\\s*\\(",
    "\\bgeom_",
    "\\bplotly\\s*\\(",
    "\\bplot_ly\\s*\\(",
    "\\blattice::",
    "\\bxyplot\\s*\\(",
    "\\bcloud\\s*\\(",
    "\\bwireframe\\s*\\(",
    "\\bgrid\\s*\\(",
    "\\bgrid\\.",
    "\\bcowplot::plot_grid\\s*\\("
  )

  # Combine into one regex
  plot_regex <- paste(plot_patterns, collapse = "|")

  # Function to strip comments (ignoring # inside quotes)
  strip_comments <- function(line) {
    # Protect quotes first
    in_single <- FALSE
    in_double <- FALSE
    chars <- strsplit(line, "")[[1]]
    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch == "'" && !in_double) in_single <- !in_single
      if (ch == '"' && !in_single) in_double <- !in_double
      if (ch == "#" && !in_single && !in_double) {
        # truncate at comment start
        return(trimws(substr(line, 1, i - 1)))
      }
    }
    trimws(line)
  }

  # Remove comments and empty lines
  stripped <- vapply(code_lines, strip_comments, FUN.VALUE = character(1))
  stripped <- unname(stripped[nzchar(stripped)])

  # Check if any line matches a plotting pattern
  any(grepl(plot_regex, stripped, perl = TRUE, ignore.case = TRUE))
}
