#' summary.SCEMChecker
#'
#' S3 method for summarizing _SCEMChecker_ objects output by [check_submission_against_template()]).
#'
#' @param object object of class _SCEMChecker_.
#' @param ... other parameters to be passed down to specific
#'            summary functions (currently unused)
#'
#' @return Nothing (called for side effects)
#'
#' @method summary SCEMChecker
#'
#' @export
#'
summary.SCEMChecker <- function(object, ...)
{
  if(!("SCEMChecker" %in% class(object))) {
    message("\n Object is not of class 'SCEMChecker'")
    invisible(NULL)
  }

  message("\n===== SCEMChecker summary =====")
  message("List of detected problems:")

  if(any(!object$present)){
    idx <- which(!object$present)
    tmp <- object[idx, ]
    message("\nMissing chunks: ")
    for (i in seq_along(idx)){
      message(tmp$chunk[i])
    }
    message("------")
  }

  if(any(object$illegal_install)){
    idx <- which(object$illegal_install)
    tmp <- object[idx, ]
    message("\nChunks with prohibited calls to install.packages: ")
    for (i in seq_along(idx)){
      message(tmp$chunk[i])
    }
    message("------")
  }

  if(any(object$error != "")){
    idx <- which(object$error != "")
    tmp <- object[idx, ]
    message("\nChunks throwing errors:")
    for (i in seq_along(idx)){
      message(tmp$chunk[i], ":")
      message("   ", tmp$error[i])
    }
    message("------")
  }

  if(any(object$missing_vars != "")){
    idx <- which(object$missing_vars != "")
    tmp <- object[idx, ]
    message("\nChunks with missing variables:")
    for (i in seq_along(idx)){
      message(tmp$chunk[i], ":")
      message("   ", tmp$missing_vars[i])
    }
    message("------")
  }

  if(any(object$type_mismatch != "")){
    idx <- which(object$type_mismatch != "")
    tmp <- object[idx, ]
    message("\nChunks with variables of the wrong type:")
    for (i in seq_along(idx)){
      message(tmp$chunk[i], ":")
      message("   ", tmp$type_mismatch[i])
    }
    message("------")
  }

  # if(any(object$plot_expected & !object$plot_called)){
  #   idx <- which(object$plot_expected & !object$plot_called)
  #   tmp <- object[idx, ]
  #   message("\nChunks with missing plot calls")
  #   for (i in seq_along(idx)){
  #     message(tmp$chunk[i])
  #   }
  #   message("------")
  # }

  invisible(NULL)
}
