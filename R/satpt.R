#' @title Saturation point analysis
#'
#' @description Saturation point analysis of multinomial responses from a
#' survey using standard errors of the sample proportions for the responses.
#'
#' @param y Vector of values to which determine if saturation has been achieved.
#' @param by Vector of values which indicate when `y` was collected.
#' Default is `NULL`.
#' @param exclude Vector of values that should be excluded in `y` and
#' `by`. Generally, this should be used to denote missing values. Default
#' is `NA` and `NaN`.
#' @param alpha Significance level for Pearson's \eqn{\chi^2} test for
#' independence. Default is `0.05`.
#' @param threshold Saturation threshold applied to the standard errors of the
#' sample proportions. Default is `0.025`.
#' @param dimnames Character vector of names for `y` and `by` when
#' displaying the contingency table, sample proportions, and standard error
#' matrices. When `dimnames` is an unnamed vector the first entry should be name
#' of `y` variable and the second entry should be name of `by` variable. If
#' `dimnames` is a named vector, then order of the values do NOT matter, as long
#' the elements are named with `"y"` and `"by"`. Default is `NULL`.
#' @param ... Additional arguments passed to [stats::chisq.test()] for more
#' control over the \eqn{\chi^2} test for independence. `x` and `y` arguments
#' from [stats::chisq.test()] should **not** be spectified because the
#' `by` and `y` from arguments above will create a contingency matrix to
#' perform the test of independence on.
#'
#' @details The `by` argument should be specified when the responses collected
#' in `y` are a by-product of the data collection mechanism defined by the `by`
#' variable. When there is no a priori data collection mechanism defined, the
#' `by` argument should not be defined. Generally, when the data is collected
#' randomly or collected during the first data collection period, there is "no
#' a priori data collection mechanism."
#'
#' Functionality of `satpt()` depends on the limited use of [stats::ftable()].
#' More specifically, `y`, `by`, and `exclude` are directly used with
#' [stats::ftable()] to create the contingency table of the collected data. The
#' contingency table created by [stats::ftable()] is converted to a `matrix` for
#' easier use with other functions. When the `dimnames` argument is used, it
#' manipulates the attributes of the created `ftable`, which impacts the
#' dimension names of the resulting `matrix`.
#'
#' Specification of the `by` argument automatically calls for a Pearson's
#' \eqn{\chi^2} test for independence and the heterogeneity index of the sample
#' proportions to be calculated. The \eqn{\chi^2} test for independence is
#' conducted to determine if response bias is present within the responses in
#' `y` are due to the data collection periods (`by`). When response bias is
#' present the pooled standard errors of the overall sample proportions for each
#' response category is reported, the pooled standard errors account for the
#' response bias. The heterogeneity index is defined as the mean
#' absoluted difference of the sample proportions for each response category
#' within each data collection period (`by`) relative to the overall sample
#' proportions for each response category. This index reflects the average
#' deviation of the data collection period proportions from the overall sample
#' proportions. Smaller values indicate the sample proportions of the data
#' collection periods are less dissimilar. This measure is of importance when
#' response bias is present. When `by` is not specified the test for
#' independence will not be conducted and the heterogeneity index will not be
#' calculated. `satpt` assumes response bias is only possible when `by` is
#' specified. Thus, there is no need to check for response bias when `by` is
#' not specified.
#'
#' @return An object with `S3` class `"satpt"` containing 11 elements.
#' \describe{
#'  \item{`threshold`}{Saturation threshold applied to the standard errors of
#'  thesample proportions.}
#'  \item{`saturation`}{A logical value indicating whether all response
#'  categories have achieved saturation given the defined `threshold`. The value
#'  of `TRUE` indicates that saturation has been achieved while a value of
#'  `FALSE` indicates that saturation was not achieved and more data is needed
#'  to achieve saturation.}
#'  \item{`counts`}{A `matrix` containing the observed cell counts of the
#'  contigency table created by `y` and `by` if provided.}
#'  \item{`phat`}{A `matrix` containing the row-wise sample proportions for the
#'  observed contigency table (`counts`).}
#'  \item{`se`}{A `matrix` containing the standard errors for the calculated
#'  sample proportions (`phat`).}
#'  \item{`pooled_se`}{A logical value indicating whether pooled standard errors
#'  were calculated due to the presence of response bias.}
#'  \item{`alpha`}{Significance level for Pearson's \eqn{\chi^2} test for
#'  independence.}
#'  \item{`test`}{A `htest` object produced by [stats::chisq.test()] containing
#'  the results from the \eqn{\chi^2} test for independence.}
#'  \item{`N`}{Total number of observations with a response provided.}
#'  \item{`total`}{A `data.frame` with 5 variables describing the overall
#'  collected sample. The `categories` variable provides the unique
#'  categories listed in `y`. While `counts`, `phat`, and `se` provide the
#'  overall cell counts, sample proportions, and standard errors for the
#'  categories, respectively. The standard errors reported for the overall
#'  sample proportions are calculated based on the presence of response bias,
#'  which is detailed above. Finally, the `saturation` variable is a logical
#'  variable denoting whether saturation was achieved for each individual
#'  category given the saturation `threshold` defined.}
#'  \item{`hindex`}{Heterogeneity index for the sample proportions calculated by
#'  mean absolute deviation.}
#' }
#'
#' @note The returned `satpt` object will contain `NULL` values for `test` and
#' `hindex` when `by` is not specified. This is done because `satpt` assumes
#' `by` is only specified when the data is collected in intervals.
#'
#' @seealso [stats::ftable()], [stats::chisq.test()]
#'
#' @examples
#' data(ein)
#'
#' # Assuming response bias is not a possiblity
#' satpt::satpt(y = ein$q2)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible. For this example, response bias is not present,
#' # so the standard errors will be the same.
#' satpt::satpt(y = ein$q2, by = ein$wave)
#'
#' # Creating an example, where response bias is present.
#'
#' ## Simulating data
#' prob <- matrix(
#'   data = c(0.4, 0.4, 0.2, 0.1, 0.1, 0.8),
#'   nrow = 2, ncol = 3, byrow = TRUE
#' )
#' catg <- LETTERS[1:3]
#' set.seed(123)
#' dat <- satpt::simulate(
#'   n = 1, size = c(250, 100), prob = prob, categories = catg
#' )
#'
#' ## Determining saturation with response bias
#' res <- satpt::satpt(y = dat$responses1, by = dat$period)
#' summary(res)
#'
#' @rdname satpt
#' @export
satpt <- function(
    y,
    by = NULL,
    exclude = c(NA, NaN),
    alpha = 0.05,
    threshold = 0.025,
    dimnames = NULL,
    ...) {
  # Checking parameter types ####
  valid_classes <- c("character", "factor", "numeric", "logical", "integer")
  if (!(class(y) %in% valid_classes)) {
    stop("y must be a vector of values.")
  }

  if (!is.null(by)) {
    if (!(class(by) %in% valid_classes)) {
      stop("by must be a vector of values.")
    }
  }

  if (!is.null(by) && (length(y) != length(by))) {
    stop("y and by must be of the same length.")
  }

  if (!methods::is(object = alpha, class2 = "numeric") || alpha >= 1) {
    stop("alpha must be numeric less than 1.")
  }

  if (!methods::is(object = threshold, class2 = "numeric") || threshold >= 1) {
    stop("threshold must be numeric less than 1.")
  }

  if (!is.null(dimnames)) {
    if (!methods::is(object = dimnames, class2 = "character")) {
      stop("dimnames must be a character vector.")
    }
    if (length(dimnames) == 1 && !is.null(by)) {
      stop("dimnames must be of length two when by is specified.")
    }
    ndimns <- names(dimnames)
    if (!is.null(ndimns) && !(all(c("y", "by") %in% ndimns))) {
      stop("For a named dimnames vector, 'y' and 'by' must be included.")
    }
  }

  # Obtaining counts ####
  if (is.null(by)) {
    counts <- stats::ftable(y, exclude = exclude)
    if (!is.null(dimnames)) {
      tmp <- attributes(counts)
      if (is.null(names(dimnames))) {
        names(tmp$col.vars) <- dimnames[1]
      } else {
        names(tmp$col.vars) <- unname(dimnames["y"])
      }
      attributes(counts) <- tmp
    }
  } else {
    counts <- stats::ftable(y ~ by, exclude = exclude)
    if (!is.null(dimnames)) {
      tmp <- attributes(counts)
      if (is.null(names(dimnames))) {
        names(tmp$col.vars) <- dimnames[1]
        names(tmp$row.vars) <- dimnames[2]
      } else {
        names(tmp$col.vars) <- unname(dimnames["y"])
        names(tmp$row.vars) <- unname(dimnames["by"])
      }
      attributes(counts) <- tmp
    }
  }
  ## Forcing ftable to be a matrix ####
  counts <- as.matrix(counts)

  # Calculating row-wise sample proportions ####
  phat <- base::proportions(counts, margin = 1)

  # Calculating row-wise standard errors of sample proportions ####
  se <- phat
  nn <- rowSums(counts)
  for (i in seq_len(nrow(se))) {
    se[i, ] <- sqrt((phat[i, ] * (1 - phat[i, ])) / nn[i])
  }

  # Conducting test of independence ####
  if (all(dim(counts) >= 2)) {
    # Specifying default testing paremters ####

    ## Specifying testing parameters ####
    test_args <- list(
      x = counts
    )

    ## Pulling additional testing parameters from user ####
    new_test_args <- list(...)

    ### Removing x or y from new parameters ####
    w_args <- which(names(new_test_args) %in% c("x", "y"))

    if (length(w_args) > 0) {
      new_test_args <- new_test_args[[-w_args]]
    }

    ## Updating default testing parameters ####
    if (length(new_test_args)) {
      test_args[names(new_test_args)] <- new_test_args
    }

    # Running test for independence ####
    test <- do.call(what = "chisq.test", args = test_args)

    ## Assigning data.name to be based on y and by parameters ####
    test$data.name <- paste0(
      names(dimnames(counts))[2],
      " given ",
      names(dimnames(counts))[1]
    )
  } else {
    test <- NULL
  }

  # Calculations for total sample statistics ####

  ## Initializing total matrix ####
  total <- data.frame(
    categories = dimnames(counts)[[2]],
    counts = NA,
    phat = NA,
    se = NA,
    saturation = NA
  )

  ## Obtaining overall observed counts ####
  total$counts <- as.integer(colSums(x = counts))

  ## Obtaining entire sample size
  total_obs <- sum(total$counts)

  ## Calculating overall sample proportions ####
  total$phat <- total$counts / total_obs

  ## Calculating overall standard errors of sample proportions
  if (!is.null(test)) {
    pooled_se <- test$p.value <= alpha
    if (pooled_se) {
      weights <- nn / sum(nn)
      for (j in seq_len(ncol(se))) {
        total$se[j] <- sqrt(sum(weights^2 * se[, j]^2))
      }
    } else {
      total$se <- sqrt((total$phat * (1 - total$phat)) / total_obs)
    }
  } else {
    pooled_se <- FALSE
    total$se <- as.vector(se)
  }

  # Determining if saturation is achieved ####

  ## Category-level saturation ####
  total$saturation <- total$se <= threshold

  ## Overall-level saturation ####
  saturation <- all(total$saturation)

  # Calculating heterogeneity index ####
  # Mean absolute deviation away from overall sample proportions
  if (!is.null(test)) {
    hindex <- rep(x = NA, length.out = dim(total)[1])
    names(hindex) <- total$categories
    for (j in seq_len(ncol(phat))) {
      hindex[j] <- sum(abs(phat[, j] - total$phat[j])) / dim(counts)[1]
    }
  } else {
    hindex <- NULL
  }

  # Output ####
  out <- vector(mode = "list", length = 11)
  names(out) <- c(
    "threshold",
    "saturation",
    "counts",
    "phat",
    "se",
    "pooled_se",
    "alpha",
    "test",
    "N",
    "total",
    "hindex"
  )
  out$threshold <- threshold
  out$saturation <- saturation
  out$counts <- counts
  out$phat <- phat
  out$se <- se
  out$pooled_se <- pooled_se
  out$alpha <- alpha
  if (!is.null(test)) {
    out$test <- test
  }
  out$N <- total_obs
  out$total <- total
  if (!is.null(hindex)) {
    out$hindex <- hindex
  }
  return(structure(out, class = "satpt"))
}
