#' Compare values
#'
#' Compares values and returns informative messages if there are differences
#'
#' @param sub_val submitted value
#' @param ref_val reference value
#' @param tol tolerance for numerical comparisons
#'
#'
#' @returns a list object with fields match (logical), detail (string)
#'

compare_values <- function(sub_val, ref_val, tol = 1e-6) {

  # 1) both are numeric (including integers)
  if (is.numeric(sub_val) && is.numeric(ref_val)) {
    cmp <- all.equal(as.numeric(sub_val),
                     as.numeric(ref_val),
                     tolerance = tol,
                     check.attributes = FALSE)
    return(list(match = isTRUE(cmp), detail = if (isTRUE(cmp)) "" else as.character(cmp)))
  }

  # both are data.frames
  if (is.data.frame(sub_val) && is.data.frame(ref_val)) {
    # dimension checks
    if (!identical(dim(sub_val), dim(ref_val))) {
      return(list(match = FALSE, #
                  detail = paste0("Dimension mismatch: sub ",
                                  paste0(dim(sub_val), collapse = "x"),
                                  " vs ref ",
                                  paste0(dim(ref_val), collapse = "x"))))
    }

    cmp <- all.equal(sub_val,
                     ref_val,
                     check.attributes = FALSE)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else as.character(cmp)))
  }

  # lists
  if (is.list(sub_val) && is.list(ref_val)) {

    cmp <- all.equal(sub_val,
                     ref_val,
                     check.attributes = FALSE)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else as.character(cmp)))
  }

  # logical
  if (is.logical(sub_val) && is.logical(ref_val)) {

    cmp <- all.equal(sub_val,
                     ref_val,
                     check.attributes = FALSE)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else as.character(cmp)))
  }

  # default: use identical()
  match <- identical(sub_val, ref_val)

  if (match) return(list(match = TRUE, detail = ""))

  # fallback: try all.equal and coerce to string
  cmp <- all.equal(sub_val,
                   ref_val,
                   check.attributes = FALSE)

  return(list(match = isTRUE(cmp),
              detail = if (isTRUE(cmp)) "" else as.character(cmp)))
}
