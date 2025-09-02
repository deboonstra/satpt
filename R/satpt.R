#' @title Saturation point analysis
#'
#' @description Saturation point analysis of multinomial responses from a
#' survey using standard errors of the sample proportions for the responses.
#'
#' @param y Multinomial responses collected and being examined for saturation.
#' See the *Details* section for valid *R* data objects and how
#' they are handled.
#' @param by Values indicating when the multinomial responses (`y`) were
#' collected. See the *Details* section for when to specify this argument.
#' @param exclude Vector of values that should be excluded in `y` and
#' `by`. Generally, this should be used to denote missing values. Default
#' is `NA` and `NaN`.
#' @param alpha Significance level for test for independence by `y` and `by`.
#' Default is `0.05`.
#' @param threshold Saturation threshold applied to the maximum standard error
#' of the sample proportions. Default is `0.025` and the threshold must be less
#' or equal to 0.25.
#' @param dimnames Character vector of names for `y` and `by` when
#' displaying the contingency table, sample proportions, and standard error
#' matrices. When `dimnames` is an unnamed vector the first entry should be name
#' of `y` variable and the second entry should be name of `by` variable. If
#' `dimnames` is a named vector, then order of the values does NOT matter, as
#' long the elements are named with `"y"` and `"by"`. Default is `NULL`.
#' @param ... Additional arguments passed to [stats::chisq.test()] or
#' [stats::fisher.test()] for more control over the test for independence.
#' `x` and `y` arguments from [stats::chisq.test()] or [stats::fisher.test()]
#' should **not** be spectified because the `by` and `y` from arguments above
#' will create a contingency matrix to perform the test of independence on.
#'
#' @details The `by` argument should be specified when the responses collected
#' in `y` are a by-product of the data collection mechanism defined by the `by`
#' variable. When there is no a priori data collection mechanism defined, the
#' `by` argument should not be defined. Generally, when the data is collected
#' randomly or collected during the first data collection period, there is "no
#' a priori data collection mechanism."
#'
#' The parameters `y` and `by` maybe a `vector`, `factor`, `matrix`,
#' `data.frame`, `data.table`, `tibble`, or `list`. When the parameters are a
#' `list` object, each element of the object should be of equal length.
#' Otherwise missing values will be appended to the end of each element based on
#' the element that has the longest length. If `y` or `by` are `factor`s the
#' underlying order of the factor will be ignored when the contingency table is
#' created. The [satpt::char_matrix()] function is used within `satpt()` to
#' coerce the values of `y` and `by` to be character values to have consistency
#' of value types.
#'
#' Generally, `y` should only have more than 1 column when select all apply
#' questions are being examined. See the `select-all-apply` vignette for more
#' information.
#'
#' Functionality of `satpt()` depends on the limited use of [stats::ftable()].
#' More specifically, `y`, `by`, and `exclude` are directly used with
#' [stats::ftable()] to create the contingency table of the collected data. The
#' contingency table created by [stats::ftable()] is converted to a `matrix` for
#' easier use with other functions. When the `dimnames` argument is used, it
#' manipulates the attributes of the created `ftable`, which impacts the
#' dimension names of the resulting `matrix`.
#'
#' Specification of the `by` argument automatically calls for a test for
#' independence and the heterogeneity index of the sample proportions to be
#' calculated. The test for independence is conducted to determine if response
#' bias is present within the responses in `y` are due to the data collection
#' periods (`by`). Fisher's Exact Test or the \eqn{I \times J} variant is
#' implemented when the more than 20% of the expected cell counts are less than
#' 5. Otherwise Pearsons' \eqn{\chi^2} Test for Independence is implemented.
#'
#' When response bias is present the pooled standard errors of the overall
#' sample proportions for each response item is reported, the pooled
#' standard errors account for the response bias. The heterogeneity index is
#' defined as the mean absoluted difference of the sample proportions for each
#' response item within each data collection period (`by`) relative to the
#' overall sample proportions for each response item. This index reflects
#' the average deviation of the data collection period proportions from the
#' overall sample proportions. Smaller values indicate the sample proportions of
#' the data collection periods are less dissimilar. This measure is of
#' importance when response bias is present. When `by` is not specified the test
#' for independence will not be conducted and the heterogeneity index will not
#' be calculated. `satpt` assumes response bias is only possible when `by` is
#' specified. Thus, there is no need to check for response bias when `by` is
#' not specified.
#'
#' Determination of saturation is based on the the response item and/or
#' collection of responses that has the largest standard error. If this largest
#' standard error achieves saturation then all other categories or responses
#' will achieve saturation. For select all apply questions, the collection of
#' responses that have the largest standard error (i.e., a sample proportion
#' closest to 0.5) will be used to determine saturation of all responses.
#'
#' @return An object with `S3` class `"satpt"` containing 12 elements. The
#' return elements in a `"satpt"` object are based on the response item that
#' had the largest standard error. The `which_saturation` returned value
#' indicates which response item had the largest standard error. This nature
#' of `satpt` is of most important when determining saturation for select all
#' apply questions.
#' \describe{
#'  \item{`threshold`}{Saturation threshold applied to the standard errors of
#'  thesample proportions.}
#'  \item{`saturation`}{A logical value indicating whether response item
#'  with the largest standard error has achieved saturation given the defined
#' `threshold`. The value of `TRUE` indicates that saturation has been achieved
#'  while a value of `FALSE` indicates that saturation was not achieved and more
#'  data is needed to achieve saturation.}
#'  \item{`which_saturation`}{A character value indicating which collection of
#'  responses within `y` determined saturation achievement. Generally, this is
#'  only of importance when examining select all apply questions. For
#'  multiple choice type of question, the returned value should just be the
#'  object name.}
#'  \item{`counts`}{A `matrix` object containing the observed cell
#'  counts of the contigency table created by `y` and `by` if provided.}
#'  \item{`phat`}{A `matrix` object containing the row-wise sample
#'  proportions for the observed contigency table (`counts`).}
#'  \item{`se`}{A `matrix` object containing the standard errors for
#'  the calculated sample proportions (`phat`).}
#'  \item{`pooled_se`}{A logical value indicating whether pooled standard errors
#'  were calculated due to the presence of response bias.}
#'  \item{`alpha`}{Significance level for the test for independence.}
#'  \item{`test`}{A `htest` object produced by [stats::chisq.test()]
#'  or [stats::fisher.test()] containing the results from the test for
#'  independence.}
#'  \item{`n`}{Total number of observations with a response provided.}
#'  \item{`total`}{A `data.frame` object with 4 variables describing
#'  the overall collected sample. The `categories` variable provides the unique
#'  categories listed in `y`. While `counts`, `phat`, and `se` provide the
#'  overall cell counts, sample proportions, and standard errors for the
#'  categories, respectively. The standard errors reported for the overall
#'  sample proportions are calculated based on the presence of response bias,
#'  which is detailed above.}
#'  \item{`hindex`}{A vector of heterogeneity index values for the sample
#'  proportions calculated by mean absolute deviation.}
#' }
#'
#' @note The returned `satpt` object will contain `NULL` values for `test` and
#' `hindex` when `by` is not specified. This is done because `satpt` assumes
#' `by` is only specified when the data is collected in intervals.
#'
#' @seealso [stats::ftable()] [stats::chisq.test()]
#'
#' @examples
#' data(diagnoses)
#'
#' # Assuming response bias is not a possiblity
#' satpt::satpt(y = diagnoses$q2)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible. For this example, response bias is not present,
#' # so the standard errors will be the same.
#' satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
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
  by,
  exclude = c(NA, NaN),
  alpha = 0.05,
  threshold = 0.025,
  dimnames = NULL,
  ...
) {
  # Capture variable name as string for fallback column name ####
  var_name <- deparse(expr = substitute(expr = y))
  var_name <- gsub(
    x = var_name,
    pattern = ".*\\$",
    replacement = "",
    perl = FALSE
  )
  # Checking parameter types ####
  if (is.atomic(x = y)) {
    tmp <- try(
      expr = y <- satpt::char_matrix(y = y, cname = var_name),
      silent = TRUE
    )
  } else {
    tmp <- try(expr = y <- satpt::char_matrix(y = y), silent = TRUE)
  }
  if (inherits(x = tmp, what = "try-error")) {
    stop(
      paste0(
        "y must be a vector, matrix, data.frame, data.table, tibble, factor,",
        " or list."
      )
    )
  }

  if (!missing(by)) {
    # Capture variable name as string for fallback column name ####
    var_name_by <- deparse(expr = substitute(expr = by))
    var_name_by <- gsub(
      x = var_name_by,
      pattern = ".*\\$",
      replacement = "",
      perl = FALSE
    )
    # by is transformed into a vector for ease of use
    tmp <- try(
      expr = by <- as.character(satpt::char_matrix(y = by)),
      silent = TRUE
    )
    if (inherits(x = tmp, what = "try-error")) {
      stop(
        paste0(
          "by must be a vector, matrix, data.frame, data.table, tibble,",
          " factor, or list."
        )
      )
    }
  } else {
    by <- NULL
  }

  if (!is.null(by) && (nrow(y) != length(by))) {
    stop("y and by must have the same number of observations.")
  }

  if (!inherits(x = alpha, what = "numeric") || alpha >= 1 || alpha <= 0) {
    stop("alpha must be numeric between 0 and 1.")
  }

  if (
    !inherits(x = threshold, what = "numeric") ||
      threshold >= 0.25 ||
      threshold <= 0
  ) {
    stop("threshold must be numeric between 0 and 0.25.")
  }

  if (!is.null(dimnames)) {
    if (!inherits(x = dimnames, what = "character")) {
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

  # Capturing additional arguments from ... ####
  capture_dots <- function(...) {
    dots <- list(...)
    return(dots)
  }

  # Obtaining counts for each column of y ####
  if (is.null(by)) {
    counts <- lapply(
      X = seq_len(ncol(y)),
      FUN = function(j) {
        # Sorting data by alphabetic order for consistency ####
        out <- y[, j][order(y[, j])]

        # Calculating counts ####
        out <- stats::ftable(y = out)

        # Providing dimension names when provided ####
        tmp <- attributes(out)
        if (!is.null(dimnames)) {
          if (is.null(ndimns)) {
            names(tmp$col.vars) <- paste0("y: ", dimnames[1])
          } else {
            names(tmp$col.vars) <- paste0("y: ", unname(dimnames["y"]))
          }
        } else {
          names(tmp$col.vars) <- paste0("y: ", colnames(y)[j])
        }
        attributes(out) <- tmp

        # Transforming counts to matrix ####
        out <- as.matrix(out)

        # Returning counts ####
        return(out)
      }
    )
  } else {
    counts <- lapply(
      X = seq_len(ncol(y)),
      FUN = function(j) {
        # Sorting data by alphabetic order for consistency ####
        out <- y[, j][order(y[, j], by)]
        by_sort <- by[order(y[, j], by)]

        # Calculating counts ####
        # by_sort is first in this list, so the analysis is perform on y (out)
        # given by (by_sort)
        out <- stats::ftable(by_sort, out, exclude = exclude)

        # Providing dimension names when provided ####
        tmp <- attributes(out)
        if (!is.null(dimnames)) {
          if (is.null(ndimns)) {
            names(tmp$col.vars) <- paste0("y: ", dimnames[1])
            names(tmp$row.vars) <- paste0("by: ", dimnames[2])
          } else {
            names(tmp$col.vars) <- paste0("y: ", unname(dimnames["y"]))
            names(tmp$row.vars) <- paste0("by: ", unname(dimnames["by"]))
          }
        } else {
          names(tmp$col.vars) <- paste0("y: ", colnames(y)[j])
          names(tmp$row.vars) <- paste0("by: ", var_name_by)
        }
        attributes(out) <- tmp

        # Transforming counts to matrix ####
        out <- as.matrix(out)

        # Returning counts ####
        return(out)
      }
    )
  }

  ## Adding names to list of counts ####
  names(counts) <- colnames(y)

  # Calculating row-wise sample proportions for each column of y ####
  phat <- lapply(
    X = counts,
    FUN = function(x) {
      base::proportions(x = x, margin = 1)
    }
  )

  # Calculating row-wise standard errors of sample proportions ####
  # for each column of y
  se <- lapply(
    X = counts,
    FUN = function(x) {
      # Calculating number of responses for each row ####
      nn <- rowSums(x)

      # Calculating row-wise sample proportions ####
      phat <- base::proportions(x = x, margin = 1)

      # Initialize standard error output ####
      out <- phat

      # Calculating standard errors ####
      for (i in seq_len(nrow(out))) {
        out[i, ] <- sqrt((phat[i, ] * (1 - phat[i, ])) / nn[i])
      }

      # Returning standard errors ####
      return(out)
    }
  )

  # Conducting test of independence for each column of y ####
  if (all(sapply(X = counts, FUN = function(x) all(dim(x = x) >= 2)))) {
    test <- lapply(
      X = counts,
      FUN = function(z) {
        # Calculated expected values ####
        expected <- outer(X = rowSums(z), Y = colSums(z)) / sum(z)

        ## Determining percentage of expected less than 5 ####
        pct_less5 <- sum(expected < 5) / (nrow(expected) * ncol(expected))

        # Specifying default testing paremters ####

        ## Specifying testing parameters ####
        if (pct_less5 < 0.2) {
          # Arguemnts for chi-squared test
          test_args <- list(
            x = z,
            correct = FALSE
          )
        } else {
          # Arguments for Fisher's Exact test
          if (any(dim(z) > 2)) {
            test_args <- list(
              x = z,
              simulate.p.value = TRUE
            )
          } else {
            test_args <- list(
              x = z
            )
          }
        }

        ## Pulling additional testing parameters from user ####
        new_test_args <- capture_dots()

        ### Removing x or y from new parameters ####
        w_args <- which(names(new_test_args) %in% c("x", "y"))

        if (length(w_args) > 0) {
          new_test_args <- new_test_args[[-w_args]]
        }

        ## Updating default testing parameters ####
        if (length(new_test_args) > 0) {
          test_args[names(new_test_args)] <- new_test_args
        }

        # Running test for independence ####
        if (pct_less5 < 0.2) {
          out <- do.call(what = "chisq.test", args = test_args)
        } else {
          out <- do.call(what = "fisher.test", args = test_args)
        }

        ## Assigning data.name to be based on y and by parameters ####
        out$data.name <- paste0(
          names(dimnames(z))[2],
          " given ",
          names(dimnames(z))[1]
        )

        # Returning test results
        return(out)
      }
    )
  } else {
    test <- vector(mode = "list", length = length(counts))
  }

  # Determing whether pooled SE are needed ####
  pooled_se <- lapply(
    X = test,
    FUN = function(tt) {
      if (!is.null(tt)) {
        out <- tt$p.value <= alpha
      } else {
        out <- NULL
      }

      return(out)
    }
  )

  # Calculations for total sample statistics for each column of y ####
  total <- Map(
    f = function(cc, tt, ss, pp) {
      # Initializing total output data.frame ####
      out <- data.frame(
        categories = dimnames(cc)[[2]],
        counts = NA,
        phat = NA,
        se = NA
      )

      # Obtaining overall observed counts ####
      out$counts <- as.integer(colSums(x = cc))

      # Obtaining entire sample size
      total_obs <- sum(out$counts)

      # Calculating overall sample proportions ####
      out$phat <- out$counts / total_obs

      # Calculating overall standard errors of sample proportions
      if (!is.null(tt)) {
        if (pp) {
          weights <- rowSums(cc) / sum(rowSums(cc))
          for (j in seq_len(ncol(ss))) {
            out$se[j] <- sqrt(sum(weights^2 * ss[, j]^2))
          }
        } else {
          out$se <- sqrt((out$phat * (1 - out$phat)) / total_obs)
        }
      } else {
        out$se <- as.vector(ss)
      }

      # Returning total output object ####
      return(out)
    },
    counts,
    test,
    se,
    pooled_se
  )

  # Finding the maximum standard error ####
  max_se <- sapply(
    X = total,
    FUN = function(z) {
      # Maximum standard error ####
      out <- max(z$se)
      return(out)
    }
  )

  # Determining if saturation is achieved ####
  saturation <- max(max_se) <= threshold
  which_saturation <- names(max_se)[which.max(max_se)]

  # Calculating heterogeneity index for each column of y ####
  # Mean absolute deviation away from overall sample proportions
  if (all(!sapply(X = test, FUN = is.null))) {
    hindex <- Map(
      f = function(cc, pp, tt) {
        # Initializing hindex output object ####
        out <- rep(x = NA, length.out = dim(tt)[1])
        names(out) <- tt$categories

        # Calculating hindex ####
        for (j in seq_len(ncol(pp))) {
          out[j] <- sum(abs(pp[, j] - tt$phat[j])) / dim(cc)[1]
        }

        # Returning hindex object ####
        return(out)
      },
      counts,
      phat,
      total
    )
  } else {
    hindex <- NULL
  }

  # Calculating total number of observations ####
  total_obs <- sapply(
    X = total,
    FUN = function(tt) {
      out <- sum(tt$counts)
      return(out)
    }
  )

  # Output ####
  out <- vector(mode = "list", length = 12)
  names(out) <- c(
    "threshold",
    "saturation",
    "which_saturation",
    "counts",
    "phat",
    "se",
    "pooled_se",
    "alpha",
    "test",
    "n",
    "total",
    "hindex"
  )
  out$threshold <- threshold
  out$saturation <- saturation
  out$which_saturation <- which_saturation
  out$counts <- counts[[which_saturation]]
  out$phat <- phat[[which_saturation]]
  out$se <- se[[which_saturation]]
  out$pooled_se <- unname(
    unlist(pooled_se)[
      names(unlist(pooled_se)) == which_saturation
    ]
  )
  out$alpha <- alpha
  if (all(!sapply(X = test, FUN = is.null))) {
    out$test <- test[[which_saturation]]
  } else {
    out$test <- NULL
  }
  out$n <- unname(total_obs[names(total_obs) == which_saturation])
  out$total <- total[[which_saturation]]
  if (!is.null(hindex)) {
    out$hindex <- hindex[[which_saturation]]
  }
  return(structure(out, class = "satpt"))
}
