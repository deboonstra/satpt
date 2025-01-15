#' @title Print saturation point analysis
#' @description `print` prints `satpt` objects created by [satpt::satpt()].
#'
#' @param x `satpt` object to be printed.
#' @param digits Minimal number of *significant digits*.
#' Default is `max(3, getOption("digits") - 3)`.
#' @inheritDotParams base::print
#'
#' @export
print.satpt <- function(x, digits = max(3, getOption("digits") - 3), ...) {
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
  names(dimnames(print_table)) <- c("Statistics", names(dimnames(x$phat))[2])

  # Printing results ####
  cat(
    "Saturation achieved? ",
    ifelse(
      test = x$saturation,
      yes = "Yes",
      no = "No"
    ),
    "\n"
  )
  cat("Overall Sample Proportions and Standard Errors\n")
  cat("==============================================\n")
  print(x = print_table, ...)
}

#' @title Saturation point analysis summary
#'
#' @description `summary` summarizes the saturation point analysis performed by
#' [satpt::satpt()].
#'
#' @param object `satpt` object to be summarized.
#' @param digits Minimal number of *significant digits*.
#' Default is `max(3, getOption("digits") - 3)`.
#'
#' @export
summary.satpt <- function(
    object, digits = max(3, getOption("digits") - 3), ...) {
  # Check object type ####
  if (!methods::is(object, "satpt")) {
    stop("object must be of satpt type.")
  }

  # Sample proportions ####
  if (nrow(object$phat) != 1) {
    phat <- object$phat

    ## Combining overall sample proportion with interval sample proportions ####
    phat <- rbind(phat, object$total[, 3])

    ## Rounding
    phat <- apply(
      X = phat,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ### Adjusting dimension names ####
    row.names(phat) <- c(row.names(object$phat), "Overall")
    colnames(phat) <- colnames(object$phat)
    names(dimnames(phat)) <- names(dimnames(object$phat))
  } else {
    phat <- object$phat

    ## Rounding
    phat <- apply(
      X = phat,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ## Converting back to matrix ####
    phat <- matrix(data = phat, nrow = 1, ncol = length(phat), byrow = TRUE)

    ### Adjusting dimension names ####
    row.names(phat) <- "Overall"
    colnames(phat) <- colnames(object$phat)
    names(dimnames(phat)) <- c("", names(dimnames(object$phat))[2])
  }

  if (nrow(object$se) != 1) {
    # Sample standard errors ####
    se <- object$se

    ## Combining overall standard errors with interval standard errors ####
    se <- rbind(se, object$total[, 4])

    ## Rounding
    se <- apply(
      X = se,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ### Adjusting dimension names ####
    row.names(se) <- c(row.names(object$se), "Overall")
    colnames(se) <- colnames(object$se)
    names(dimnames(se)) <- names(dimnames(object$se))
  } else {
    se <- object$se

    ## Rounding
    se <- apply(
      X = se,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ## Converting back to matrix ####
    se <- matrix(data = se, nrow = 1, ncol = length(se), byrow = TRUE)

    ### Adjusting dimension names ####
    row.names(se) <- "Overall"
    colnames(se) <- colnames(object$se)
    names(dimnames(se)) <- c("", names(dimnames(object$se))[2])
  }

  # Saturation by category ####
  saturation_catg <- object$total[, c(1, 5)]
  saturation_catg$saturation <- ifelse(
    test = saturation_catg$saturation,
    yes = "Yes",
    no = "No"
  )
  colnames(saturation_catg) <- c("Categories", "Saturation")

  # Heterogeneity index ####
  if (!is.null(object$hindex)) {
    hindex <- data.frame(
      categories = names(object$hindex),
      index = as.numeric(
        format(round(x = object$hindex, digits = digits), nsmall = digits)
      )
    )
  } else {
    hindex <- NULL
  }

  # Output ####
  out <- list()
  out$threshold <- object$threshold
  out$saturation <- ifelse(test = object$saturation, yes = "Yes", no = "No")
  out$saturation_catg <- saturation_catg
  out$N <- object$N
  out$phat <- phat
  out$se <- se
  out$pooled_se <- object$pooled_se
  out$alpha <- object$alpha
  out$test <- object$test
  out$hindex <- hindex
  return(structure(out, class = "summary.satpt"))
}

#' @title Print summary of saturation point analysis
#'
#' @description `print` prints the summary of the saturation point analysis
#' produced by [satpt::summary.satpt()].
#'
#' @param x `summary.satpt` object to be printed.
#' @inheritDotParams base::print
#'
#' @rdname summary.satpt
#' @export
print.summary.satpt <- function(x, ...) {
  # Check object type ####
  if (!methods::is(x, "summary.satpt")) {
    stop("x must be of summary.satpt type.")
  }

  # Printing saturation information ####
  cat("\nSaturation point analysis of sample proportions\n")
  cat("===============================================\n")
  cat(
    "\nSaturation achieved? ",
    x$saturation,
    "\nSaturation threshold of ",
    x$threshold,
    "\nResponses collected from a sample size of ",
    paste0(x$N, "\n\n"),
    sep = ""
  )

  # Sample proportions ####
  if (nrow(x$phat) != 1) {
    cat("Data interval and overall sample proportions\n")
  } else {
    cat("Overall sample proportions\n")
  }
  cat("============================================\n")
  print(x$phat, ...)

  # Standard errors ####
  if (nrow(x$se) != 1) {
    cat("\n\nData interval and overall standard errors\n")
  } else {
    cat("\n\nOverall standard errors\n")
  }
  cat("=========================================\n")
  print(x$se, ...)
  cat(
    "\nPooled standard errors? ",
    ifelse(
      test = x$pooled_se,
      yes = "Yes",
      no = "No"
    ),
    "\n"
  )

  # Test for independence ####
  if (!is.null(x$test)) {
    print(x$test, ...)
    cat(
      "Response bias present? ",
      ifelse(
        test = x$pooled_se,
        yes = "Yes",
        no = "No"
      ),
      "\nSignificance level: ",
      paste0(x$alpha, "\n"),
      sep = ""
    )
  }

  # Heterogeneity index ####
  if (!is.null(x$hindex)) {
    cat("\n\nHeterogeneity index\n")
    cat("====================\n")
    names(x$hindex) <- c("Categories", "Index")
    print(x$hindex, row.names = FALSE, ...)
  }
}
