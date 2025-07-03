#' @title Print saturation point analysis
#'
#' @description `print` prints `satpt` objects created by [satpt::satpt()].
#'
#' @param x `satpt` object to be printed.
#' @param digits Minimal number of *significant digits*.
#' Default is `max(3, getOption("digits") - 3)`.
#' @inheritDotParams base::print
#'
#' @examples
#' data(ein)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible. For this example, response bias is not present,
#' # so the standard errors will be the same.
#'
#' # Saving analysis as R object and printing
#' res <- satpt::satpt(y = ein$q2, by = ein$wave)
#' print(x = res, digits = 3)
#'
#' @export
print.satpt <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  # Check object type ####
  if (!methods::is(object = x, class2 = "satpt")) {
    stop("x must be of satpt type.")
  }

  # Creating printing object ####
  phat <- x$total$phat
  phat <- format(round(x = phat, digits = digits), nsmall = digits)
  se <- x$total$se
  se <- format(round(x = se, digits = digits), nsmall = digits)
  print_table <- matrix(
    data = as.numeric(c(phat, se)),
    nrow = 2,
    ncol = length(phat),
    byrow = TRUE,
    dimnames = list(
      c("Proportion", "SE"),
      x$total$categories
    )
  )
  names(dimnames(print_table)) <- c(
    "Statistics",
    names(dimnames(x$phat))[2]
  )

  # Printing results ####
  cat("Analysis based on:", x$which_saturation, "\n")
  cat(
    "Saturation achieved? ",
    ifelse(
      test = x$saturation,
      yes = "Yes",
      no = "No"
    ),
    "\n\n"
  )
  cat("Overall Sample Proportions and Standard Errors\n")
  cat("==============================================\n")
  print(x = print_table, ...)
}
