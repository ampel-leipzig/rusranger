#' Case weights for ROS
#'
#' @param y `factor`/`numeric`, classes
#' @param ndups `numeric(1)`, times of duplication of minority class.
#' @return `double(length(y))` with case weights
#' @noRd
.roscaseweights <- function(y, ndups) {
    w <- rep_len(1, length(y))
    my <- .minority(y)
    nm <- .nmin(y)
    w[y == my] <- (length(y) + nm * ndups) / nm
    w
}
