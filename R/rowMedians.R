#' Row-/Colwise Medians
#'
#' Row-/Colwise Medians similar to `rowMeans`.
#'
#' @param x `matrix`
#' @param na.rm `logical(1)`, should `NA` be removed?
#' @return `numeric`, medians for each row/column.
#' @rdname rowMedians
#' @export
#' @examples
#' m <- matrix(1:8, nrow = 2)
#' rowMedians(m)
rowMedians <- function(x, na.rm = FALSE) {
    mode(x) <- "numeric"
    apply(x, 1L, median, na.rm = na.rm)
}

#' @rdname rowMedians
#' @export
#' @examples
#' colMedians(m)
colMedians <- function(x, na.rm = FALSE) {
    mode(x) <- "numeric"
    apply(x, 2L, median, na.rm = na.rm)
}
