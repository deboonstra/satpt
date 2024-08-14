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
#' @param alpha Significance level for Pearson's \eqn{\chi^2} for independence.
#' Default is `0.05`.
#' @param threshold Saturation threshold applied to the standard errors. Default
#' is \code{0.05}.
#' @param names Character vector of names for `y` and `by` when
#' displaying the contingency table, sample proportions, and standard error
#' matrices. Default is `NULL`. When `names` is specified the first entry should
#' be name of `y` and the second entry should be name of `by`.
#'
#' @details PLACE HOLDER
#'
#' @return An object with `S3` class `"satpt"` containing
#' \describe{
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
#'  \item{`test`}{A `htest` object produced by [stats::chisq.test()] containing
#'  the results from the \eqn{\chi^2} for independence.}
#'  \item{`total_obs`}{Total number of observations with a response provided.}
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
#' satpt::satpt(y = ein$responses)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible.
#' satpt::satpt(y = ein$responses, by = ein$wave)
#'
#' @rdname satpt
#' @export
satpt <- function(
  y, by = NULL, exclude = c(NA, NaN), alpha = 0.05, threshold = 0.05,
  names = NULL
) {

  # Checking parameter types ####
  if (!(class(y) %in% c("character", "factor", "numeric", "logical"))) {
    stop("y must be a vector of values.")
  }

  if (!is.null(by)) {
    if (!(class(by) %in% c("character", "factor", "numeric", "logical"))) {
      stop("by must be a vector of values.")
    }
  }

  if (!is.null(by) && (length(y) != length(by))) {
    stop("y and by must be of the same length.")
  }

  if (!is.numeric(alpha)) {
    stop("alpha must be numeric.")
  }

  if (!is.numeric(threshold)) {
    stop("threshold must be numeric.")
  }

  if (!is.null(names)) {
    if (!is.character(names)) {
      stop("names must be a character vector.")
    }
    if (length(names) == 1 && !is.null(by)) {
      stop("names must be of length two when by is specified.")
    }
  }

  # Obtaining counts ####
  if (is.null(by)) {
    counts <- stats::ftable(y, exclude = exclude)
    if (!is.null(names)) {
      tmp <- attributes(counts)
      names(tmp$col.vars) <- names[1]
      attributes(counts) <- tmp
    }
  } else {
    counts <- stats::ftable(y ~ by, exclude = exclude)
    if (!is.null(names)) {
      tmp <- attributes(counts)
      names(tmp$col.vars) <- names[1]
      names(tmp$row.vars) <- names[2]
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
    if (all(dim(counts) == 2)) {
      test <- stats::chisq.test(x = counts, correct = TRUE)
    } else {
      test <- stats::chisq.test(x = counts, correct = FALSE)
    }
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
    if (test$p.value <= alpha) {
      weights <- nn / sum(nn)
      for (j in seq_len(ncol(se))) {
        total$se[j] <- sqrt(sum(weights^2 * se[, j]^2))
      }
    } else {
      total$se <- sqrt((total$phat * (1 - total$phat)) / total_obs)
    }
  } else {
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
  out <- list()
  out$saturation <- saturation
  out$counts <- counts
  out$phat <- phat
  out$se <- se
  out$test <- test
  out$N <- total_obs
  out$total <- total
  out$hindex <- hindex
  return(structure(out, class = "satpt"))
}

#' @export
print.satpt <- function(x, digits = getOption("digits"), ...) {
  # Check object type ####
  if (!methods::is(x, "satpt")) {
    stop("x must be of satpt type.")
  }

  # Creating printing object ####
  phat <- x$total$phat
  phat <- format(round(x = phat, digits = digits), nsmall = digits)
  se <- x$total$se
  se <- format(round(x = se, digits = digits), nsmall = digits)
  print_table <- matrix(
    data = as.numeric(c(phat, se)),
    nrow = 2, ncol = length(phat),
    byrow = TRUE,
    dimnames = list(
      c("Proportion", "SE"),
      x$total$categories
    )
  )

  # Printing results ####
  cat(
    "Saturation: ",
    ifelse(test = x$saturation, yes = "Yes", no = "No"),
    "\n"
  )
  cat("Overall Sample Proportions and Standard Errors\n")
  cat("----------------------------------------------\n")
  cat("----------------------------------------------\n")
  print(x = print_table, ...)
}
