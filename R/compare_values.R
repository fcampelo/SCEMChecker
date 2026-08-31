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
#' @importFrom dplyr %>% mutate across select

compare_values <- function(sub_val, ref_val, tol = 1e-6) {

  if (is.numeric(sub_val) && is.numeric(ref_val)) { # 1) both are numeric (including integers)
    cmp <- all.equal(as.numeric(sub_val),
                     as.numeric(ref_val),
                     tolerance = tol,
                     check.attributes = FALSE)
    return(list(match = isTRUE(cmp), detail = if (isTRUE(cmp)) "" else as.character(cmp)))

  } else if (is.data.frame(sub_val) && is.data.frame(ref_val)) {   # both are data.frames
    # # dimension checks
    # if (!identical(dim(sub_val), dim(ref_val))) {
    #   return(list(match = FALSE, #
    #               detail = paste0("Dimension mismatch: sub ",
    #                               paste0(dim(sub_val), collapse = "x"),
    #                               " vs ref ",
    #                               paste0(dim(ref_val), collapse = "x"))))
    # }

    sub_val <- sub_val %>%
      mutate(across(where(is.factor), as.character)) %>%
      as.data.frame()


    ref_val <- ref_val %>%
      mutate(across(where(is.factor), as.character)) %>%
      as.data.frame()

    eqlist <- logical(ncol(ref_val))
    for (nn in 1:ncol(ref_val)){
      colname <- names(ref_val)[nn]
      if (colname %in% names(sub_val)){
        cres <- all.equal(sort(ref_val[[colname]]), sort(sub_val[[colname]]), tolerance = 1e-6)
        eqlist[nn] <- ifelse(is.logical(cres), cres, FALSE)
      } else {
        eqlist[[nn]] <- FALSE
      }
    }

    cmp <- all(eqlist)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else data.frame(col = names(ref_val), equal = eqlist)))


  } else if (is.list(sub_val) && is.list(ref_val)) { # lists
    cmp <- all.equal(sub_val,
                     ref_val,
                     check.attributes = FALSE)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else as.character(cmp)))

  } else if (is.logical(sub_val) && is.logical(ref_val)) { # logical
    cmp <- all.equal(sub_val,
                     ref_val,
                     check.attributes = FALSE)

    return(list(match = isTRUE(cmp),
                detail = if (isTRUE(cmp)) "" else as.character(cmp)))
  } else {

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
}
