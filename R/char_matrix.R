#' Coerce Input to Character Matrix with Meaningful Column Names
#'
#' Converts various *R* object types, such as vectors, factors, data frames,
#' data tables, tibbles, and lists, into a character matrix, preserving or
#' assigning column names intelligently.
#'
#' @param y An object to be converted to a character matrix. Acceptable types
#' include `vector`, `factor`, `matrix`, `data.frame`, `data.table`, `tibble`,
#' or `list`.
#' @param cname A character vector of column names. Default is `NULL`, which
#' means `factor`s and `atomic` vectors will not be assigned a column name.
#'
#' @details The transformation of the *R* objects into a character matrix is
#' done through [base::as.character] and using [base::as.matrix] when the
#' method is defined for the object type. For `list`s, `NA` are appended to the
#' elements that have an object length less than the maximum object length.
#'
#' @return A `character` matrix with meaningful column names when possible.
#'
#' @seealso [base::as.matrix] [data.table::data.table] [tibble::tibble]
#'
#' @examples
#' char_matrix(y = 1:3)
#' char_matrix(y = factor(x = c("a", "b", "c")), cname = "ex_name")
#' char_matrix(y = data.frame(a = 1:3, b = letters[1:3]))
#' char_matrix(y = list(one = 1:3, two = 4:6))
#'
#' @export
char_matrix <- function(y, cname = NULL) {
  # Helper: Null-coalescing operator
  `%||%` <- function(a, b) {
    if (!is.null(x = a)) a else b
  }

  # Handle factor or character vector ####
  if (is.factor(x = y)) {
    y <- as.character(x = y)
  }

  # Simple vector to single-column character matrix ####
  if (is.atomic(x = y) && is.vector(x = y)) {
    out <- matrix(data = as.character(x = y), ncol = 1)
    if (!is.null(cname) && length(cname) == 1) {
      colnames(x = out) <- cname
    }
    return(out)
  }

  # List input: handle unequal-length elements with padding ####
  if (
    is.list(x = y) && !inherits(x = y, what = c("data.frame", "data.table"))
  ) {
    # Creating padded character list ####
    max_len <- max(sapply(X = y, FUN = length))
    padded <- lapply(
      X = y,
      FUN = function(yy) {
        col_char <- as.character(x = yy)
        length(x = col_char) <- max_len # pads with NA automatically
        return(col_char)
      }
    )

    # Creating character matrix ####
    out <- do.call(what = "cbind", args = padded)
    if (!is.null(cname) && length(cname) == ncol(out)) {
      colnames(x = out) <- cname
    } else {
      colnames(x = out) <- names(x = y) %||% paste0("V", seq_along(padded))
    }
    return(out)
  }

  # data.frame, data.table, or tibble ####
  if (inherits(x = y, what = c("data.frame", "data.table"))) {
    out <- as.matrix(
      x = data.frame(
        lapply(X = y, FUN = as.character),
        stringsAsFactors = FALSE
      )
    )
    if (!is.null(cname) && length(cname) == ncol(out)) {
      colnames(x = out) <- cname
    } else {
      colnames(x = out) <- colnames(x = y)
    }
    return(out)
  }

  # Matrix input ####
  if (is.matrix(x = y)) {
    out <- apply(X = y, MARGIN = c(1, 2), FUN = as.character)
    if (!is.null(cname) && length(cname) == ncol(out)) {
      colnames(x = out) <- cname
    } else {
      colnames(x = out) <- colnames(x = y)
    }
    return(out)
  }

  # Throwing an error if coercion fails ####
  stop(
    paste0(
      "x must be a vector, matrix, data.frame, data.table, tibble, factor,",
      " or list."
    )
  )
}
