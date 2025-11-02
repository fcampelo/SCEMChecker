# ========== Helper: Parse required variables/plots from code ==========
parse_requirements <- function(code,
                               reqvars_str = "Required variables",
                               reqplot_str = "Required plots") {
  req_vars <- list()
  req_plot <- NULL

  req_line  <- grep(paste0("^#'\\s*", reqvars_str, ":"), code, value = TRUE)
  plot_line <- grep(paste0("^#'\\s*", reqplot_str, ":"), code, value = TRUE)

  if (length(req_line) > 0) {
    var_part <- sub(paste0("^#'\\s*", reqvars_str, ":\\s*"), "", req_line)
    var_defs <- strsplit(var_part, ";")[[1]]

    for (def in var_defs) {
      def <- stringr::str_trim(def)

      if (nchar(def) == 0) next

      m <- stringr::str_match(def, "([^\\(]+)\\(([^\\)]+)\\)")

      if (!is.na(m[1, 1])) {
        vname  <- stringr::str_trim(m[1, 2])
        vclass <- stringr::str_trim(m[1, 3])

        req_vars[[vname]] <- vclass
      }
    }
  }
  if (length(plot_line) > 0) {
    req_plot <- sub(paste0("^#'\\s*", reqplot_str, ":\\s*"), "", plot_line)

    if (req_plot == "") req_plot <- NULL
  }

  list(vars = req_vars, plot = req_plot)
}
