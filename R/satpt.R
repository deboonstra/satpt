#' @title Saturation point analysis
#'
#' @description Saturation point analysis of multinomial responses from a
#' survey using standard errors of the sample proportions for the responses.
#'
#' @param y Vector of values to which determine if saturation has been achieved.
#' @param by Vector of values which indicate when \code{y} was collected.
#' Default is \code{NULL}.
#' @param exclude Vector of values that should be excluded in \code{y} and
#' \code{by}. Generally, this should be used to denote missing values. Default
#' is \code{NA} and \code{NaN}.
#' @param alpha Significance level for Pearson's \eqn{\chi^2} for independence.
#' Default is \code{0.05}.
#' @param threshold Saturation threshold applied to the standard errors. Default
#' is \code{0.05}.
#' @param names Character vector of names for \code{y} and \code{by} when
#' displaying \code{ftable} created by \code{stats::ftable}. Default is
#' \code{NULL}.
#'
#' @seealso \code{\link{stats::ftable()}}
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
    if (length(names) == 2 && is.null(by)) {
      stop("names must be of length one when by is not specified.")
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
      names(tmp$col.vars) <- names
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

  # Calculating row-wise sample proportions ####
  phat <- base::prop.table(counts, margin = 1)

  # Calculating row-wise standard errors of sample proportions ####
  se <- phat
  nn <- rowSums(counts)
  names(nn) <- unname(unlist(attributes(counts)$row.vars))
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
    categories = unname(unlist(attributes(counts)$col.vars)),
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
