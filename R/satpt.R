#' @title Saturation point analysis
#' @export satpt
satpt <- function(x, ...) UseMethod("satpt")

#' @rdname satpt
#' @export
satpt.default <- function(...) {
  x <- stats::ftable(...)
  phat <- base::prop.table(x, margin = 1)
  se <- phat
  nn <- rowSums(x)
  for (i in seq_len(nrow(se))) {
    se[i, ] <- sqrt((se[i, ] * (1 - se[i, ])) / nn[i])
  }
  out <- list()
  out$counts <- x
  out$phat <- phat
  out$se <- se
  return(structure(out, class = "satpt"))
}

#' @rdname satpt
#' @export
satpt.formula <- function(...) {
  x <- stats::ftable(...)
  phat <- base::prop.table(x, margin = 1)
  se <- phat
  nn <- rowSums(x)
  for (i in seq_len(nrow(se))) {
    se[i, ] <- sqrt((se[i, ] * (1 - se[i, ])) / nn[i])
  }
  out <- list()
  out$counts <- x
  out$phat <- phat
  out$se <- se
  return(structure(out, class = "satpt"))
}

#' @rdname satpt
#' @export
print.satpt <- function(object, digits = 3) {
  # Check object type ####
  if (class(object) != "satpt") {
    stop("object must be of satpt type.")
  }
  phat <- matrix(
    data = object$phat,
    nrow = nrow(object$phat),
    ncol = ncol(object$phat)
  )
  phat <- format(round(x = phat, digits = digits), nsmall = digits)
  se <- matrix(
    data = object$se,
    nrow = nrow(object$se),
    ncol = ncol(object$se)
  )
  se <- format(round(x = se, digits = digits), nsmall = digits)
  print_table <- object$phat
  for (i in seq_len(nrow(print_table))) {
    for (j in seq_len(ncol(print_table))) {
      print_table[i, j] <- paste0(phat[i, j], " (", se[i, j], ")")
    }
  }
  cat("Sample proportions (Standard errors)\n")
  print_table
}
