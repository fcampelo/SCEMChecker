#' Get datasets for the SCEM coursework
#'
#' This function is used to instantiate datasets for activities related to the
#' EMATM0061 - Statistical Computing and Empirical Methods unit at the
#' University of Bristol.
#'
#' @param seed integer used for dataset generation. Unless otherwise stated,
#' it should be your student ID.
#' @param N the desired number of datasets to be generated.
#' @param maindata a data.frame containing the source dataset from which further
#' datasets will be instantiated. This will commonly be provided by the unit
#' director.
#' @param save.folder path to the folder where the resulting data should be saved.
#' Use `NULL` if you don't wish the data to be saved to disk.
#' @param options further options to be passed to the dataset generator. If needed,
#' these will be specified by the unit director.
#'
#' @returns a named list object containing `N` datasets generated.
#'
#' @importFrom dplyr %>% slice_sample group_by n
#'
#' @export

get_cw_datasets <- function(seed,
                            N,
                            maindata = NULL,
                            save.folder = NULL,
                            options = list(lmin = 2000,
                                           lmax = 4000,
                                           class.column = "Class",
                                           class.neg    = 0,
                                           class.pos    = 1)){

  if(!is.null(save.folder)){
    if(!exists(save.folder)){
      dir.create(save.folder, recursive = TRUE)
    }
  }

  # store old prng seed
  rs <- .Random.seed
  # set new seed for this function only
  set.seed(seed)

  maxpos <- sum(maindata[options$class.column] == options$class.pos)
  maxneg <- sum(maindata[options$class.column] == options$class.neg)

  res <- vector("list", length = N)
  names(res) <- sprintf("Dataset_%03d", 1:N)

  for (i in seq_along(res)){
    prop.pos <- round(runif(1, min = .1, max = .9), digits = 2)
    prop.neg <- 1 - prop.pos
    ds <- as.integer(runif(1, min = options$lmin, max = options$lmax))

    npos <- as.integer(min(ds * prop.pos, maxpos))
    nneg <- as.integer(min(ds * prop.neg, maxneg))

    xpos <- maindata[which(maindata[options$class.column] == options$class.pos), ] %>%
      slice_sample(n = npos)

    xneg <- maindata[which(maindata[options$class.column] == options$class.neg), ] %>%
      slice_sample(n = nneg)

    res[[i]] <- xpos %>%
      bind_rows(xneg) %>%
      slice_sample(prop = 1)

    if(!is.null(save.folder)){
      saveRDS(res[[i]], paste0(save.folder, "/", seed, "_", names(res)[i], ".rds"))
    }
  }

  # return PRNG to original state
  set.seed(rs)

  return(res)
}
