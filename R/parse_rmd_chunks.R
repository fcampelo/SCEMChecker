# ========== Helper: Parse Rmd and extract code chunks ==========
parse_rmd_chunks <- function(path) {
  lines  <- readLines(path, warn = FALSE)
  starts <- grep("^```\\{r", lines)
  ends   <- grep("^```\\s*$", lines)

  if (length(starts) != length(ends))
    stop("Unbalanced code chunks in ", path)

  chunks <- list()
  for (i in seq_along(starts)) {
    start  <- starts[i]
    end    <- ends[i]
    header <- lines[start]
    name   <- stringr::str_match(header, "\\{r\\s+([^,\\s\\}]*)")[, 2]

    if (is.na(name)) next

    code   <- lines[(start + 1):(end - 1)]
    chunks[[name]] <- code
  }

  return(chunks)
}
