#' @title Simulation of multinomial responses
#'
#' @description Simulating the data of multinomial responses collected during a
#' survey.
#'
#' @param n Integer specifying the number of random samples to draw.
#' @param size Vector of integer values, say \eqn{N_i}, specifying the
#' sample sizes for \eqn{i = 1, \ldots , I} data collection periods.
#' @param prob Numeric non-negative vector or matrix specifying the
#' probabilities for the \emph{K} response categories in the \eqn{I} data
#' collection periods.
#' @param categories Vector of values specifying the \emph{K} category labels to
#' be represented. Default is `NULL`.
#'
#' @details Running in the backend is [stats::rmultinom()]. So, see the
#' documentation for [stats::rmultinom()]for more information on how the
#' multinomial responses are generated.
#'
#' The arguments `size` and `prob` are directly connect when it comes to
#' generating multinomial responses for multiple data collection periods. `prob`
#' may either be a vector or matrix specifying probabilities for the \emph{K}
#' response categories. When `size` is specified to generate data for multiple
#' data collection periods, then `prob` must be specified as a numeric vector of
#' length `length(size)` times \emph{K} or numeric matrix with `length(size)`
#' rows and \emph{K} columns, where each row of the matrix specify the \emph{K}
#' probabilities of each data collection period.
#'
#' When `categories` is not defined (`categories = NULL`) `simulate` will
#' create default lables of \emph{Category k, } for \eqn{k = 1, \ldots , K}.
#'
#' @return A `data.frame` with \eqn{N = \sum_{i}N_{i}} rows and `n + 1`
#' variables, which are described below.
#' \describe{
#'  \item{`period`}{Data collection period defined by the length of `size`}
#'  \item{`responses1 ... n`}{Simulated multinomial responses with either the
#'  values defined by `categories` or \emph{Category k, } for
#'  \eqn{k = 1, \ldots , K}}
#' }
#'
#' @seealso [stats::rmultinom()]
#'
#' @examples
#' # Creating 5 simulated data sets of with a sample size of 10, where there
#' # are two possible response categories with a 50% chance of being selected.
#' satpt::simulate(n = 5, size = 10, prob = c(0.5, 0.5))
#'
#' # Creating 1 simulated data set for two data collection periods, where there
#' # are three possible response categories that are labeled.
#' prob <- matrix(
#'  data = c(0.4, 0.4, 0.2, 0.1, 0.1, 0.8),
#'  nrow = 2, ncol = 3, byrow = TRUE
#' )
#' catg <- LETTERS[1:3]
#' satpt::simulate(n = 1, size = c(20, 10), prob = prob, categories = catg)
#'
#' @export
simulate <- function(n, size, prob, categories = NULL) {
  # checking parameter types ####
  if (n %% floor(n) != 0) {
    stop("n must be an integer specifying number of simulations.")
  }
  if (all(size %% floor(size) != 0)) {
    stop("size must be integer valued specifying the sample size.")
  }
  if (!(all(prob >= 0) || all(prob <= 1))) {
    stop("probs must contain numeric values between 0 and 1.")
  }

  # Generating simulated data sets ####

  ## Forcing response category probabilities to be a matrix ####
  if (!is.matrix(prob)) {
    prob <- matrix(
      data = prob,
      nrow = length(size), ncol = length(prob) / length(size),
      byrow = TRUE
    )
  } else {
    prob <- prob
  }

  ### Checking that probabilities sum to 1 ####
  sum_to_one <- apply(
    X = prob,
    MARGIN = 1,
    FUN = function(x) {
      test <- sum(x)
      test <- test == 1
      return(test)
    }
  )

  if (!all(sum_to_one)) {
    stop("Probabilities in prob must sum to one for each size.")
  }

  ### Checking categories listed match number of probabilities ####
  if (!is.null(categories)) {
    if (length(categories) != ncol(prob)) {
      stop("Length of categories must be length of probability classes.")
    }
  }

  ## Generating data ####

  ### Initialzing ####
  data <- vector(mode = "list", length = nrow(prob))

  ### Simulating ####
  for (k in seq_along(size)) {

    #### Creating temporary data set ####
    tmp <- data.frame(matrix(data = NA, nrow = size[k], ncol = n + 1))
    colnames(tmp) <- c("period", paste0("responses", seq_len(n)))

    #### Assigning data collection period indicator #####
    tmp$period <- k

    #### Obtaining sample counts for categories ####
    counts <- stats::rmultinom(n = n, size = size[k], prob = prob[k, ])

    #### Assigning categories ####
    if (is.null(categories)) {
      categories <- paste0("Category ", seq_len(nrow(counts)))
    } else {
      categories <- categories
    }
    for (j in 2:ncol(tmp)) {
      tmp[, j] <- rep(x = categories, times = counts[, (j - 1)])
    }
    data[[k]] <- tmp
  }

  ### Combining data to one data.frame ####
  data <- do.call("rbind", data)

  # Output ####
  return(data)
}
