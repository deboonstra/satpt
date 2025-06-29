#' Coerce Input to Character Matrix with Meaningful Column Names
#'
#' Converts various *R* object types, such as vectors, factors, data frames,
#' data tables, tibbles, and lists, into a character matrix, preserving or
#' assigning column names intelligently.
#'
#' @param x An object to be converted to a character matrix. Acceptable types
#' include `vector`, `factor`, `matrix`, `data.frame`, `data.table`, `tibble`,
#' or `list`.
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
#' char_matrix(x = 1:3)
#' char_matrix(x = factor(x = c("a", "b", "c")))
#' char_matrix(x = data.frame(a = 1:3, b = letters[1:3]))
#' char_matrix(x = list(one = 1:3, two = 4:6))
#'
#' @export
char_matrix <- function(x = x) {
  # Helper: Null-coalescing operator
  `%||%` <- function(a, b) {
    if (!is.null(x = a)) a else b
  }

  # Capture variable name as string for fallback column name ####
  var_name <- deparse(expr = substitute(expr = x))
  var_name <- gsub(
    x = var_name,
    pattern = ".*\\$",
    replacement = "",
    perl = FALSE
  )

  # Handle factor or character vector ####
  if (is.factor(x = x)) {
    x <- as.character(x = x)
  }

  # Simple vector to single-column character matrix ####
  if (is.atomic(x = x) && is.vector(x = x)) {
    out <- matrix(data = as.character(x = x), ncol = 1)
    colnames(x = out) <- var_name
    return(out)
  }

  # List input: handle unequal-length elements with padding ####
  if (
    is.list(x = x) && !inherits(x = x, what = c("data.frame", "data.table"))
  ) {
    # Creating padded character list ####
    max_len <- max(sapply(X = x, FUN = length))
    padded <- lapply(
      X = x,
      FUN = function(xx) {
        col_char <- as.character(x = xx)
        length(x = col_char) <- max_len # pads with NA automatically
        return(col_char)
      }
    )

    # Creating character matrix ####
    out <- do.call(what = "cbind", args = padded)
    colnames(x = out) <- names(x = x) %||% paste0("V", seq_along(padded))
    return(out)
  }

  # data.frame, data.table, or tibble ####
  if (inherits(x = x, what = c("data.frame", "data.table"))) {
    out <- as.matrix(
      x = data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
    )
    colnames(x = out) <- colnames(x = x)
    return(out)
  }

  # Matrix input ####
  if (is.matrix(x = x)) {
    out <- apply(X = x, MARGIN = c(1, 2), FUN = as.character)
    colnames(x = out) <- colnames(x = x)
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
