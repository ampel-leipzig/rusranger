#' SMOTE
#'
#' SMOTE: Synthetic Minority Over-sampling Technique.
#'
#' @param x `matrix`, features
#' @param y `factor`, class (length has to be equal to number of rows in `x`).
#' @param k `numeric(1)`, number of nearest neighbours.
#' @param ndups `numeric(1)`, number of synthetic duplicates to upsample
#' minority. Default is `0` which tries to upsample to majority.
#' @return `list` with elements `x` features with SMOTEd samples and `y` with
#' added minority class elements.
#'
#' @importFrom stats dist runif
#' @export
#'
#' @references
#' Chawla, Bowyer, Hall and Kegelmeyer. 2002.
#' SMOTE: Synthetic Minority Over-sampling Technique.
#' Journal of Artificial Intelligence Research 16. 321–357.
#' \doi{10.1613/jair.953}
smote <- function(x, y, k = 5L, ndups = 0L) {
    if (nrow(x) != length(y))
        stop("nrow(x) has to be equal to length(y).")
    my <- .minority(y)
    sel <- y == my
    if (!ndups)
        ndups <- .ndups(nrow(x), .nmin(y))
    sm <- .smote(x[sel,, drop = FALSE], k = k, ndups = ndups)
    x <- rbind(x, sm)
    rownames(x) <- NULL
    list(x = x, y = c(y, rep.int(my, nrow(sm))))
}

.smote <- function(x, k = 5L, ndups = 1L) {
    if (!is.matrix(x))
        stop("'x' has to be a matrix.")
    if (!length(k) == 1 || !is.integer(k) || k < 2L)
        stop("'k' has to be an integer of length 1 and greater or equal 2.")
    if (!length(ndups) == 1 || !is.integer(ndups) || ndups < 1L)
        stop("'ndups' has to be an integer of length 1 and greater 0.")
    nr <- nrow(x)
    nc <- ncol(x)
    iknn <- .knn(x, k)
    s <- matrix(, nrow = nr * ndups, ncol = nc)
    sdups <- seq_len(ndups)

    for (i in seq_len(nr)) {
        ir <- rep.int(i, ndups)
        # difference to nearest random nearest neighbour
        j <- iknn[i, sample.int(k, ndups, replace = TRUE)]
        d <- x[ir, ] - x[j, , drop = FALSE]
        # difference multiplied with random gap in range 0..1
        s[(i - 1L) * ndups + sdups,] <- x[ir, ] + runif(ndups) * d
    }
    s
}

#' K-nearest neighbour algrorithm
#'
#' Simple (not fastest) implementation of knn
#'
#' @param x `matrix`
#' @param k `integer(1)`, number of nearest neighbours
#' @return `matrix`, `nrow(x)` times `k`,
#' for each row in `x` the row indices of the nearest neighbours.
#' @noRd
.knn <- function(x, k = 5) {
    d <- dist(x, method = "euclidean")
    nr <- nrow(x)
    m <- matrix(, nrow = nr, ncol = k)
    k <- seq_len(k)
    for (i in seq_len(nrow(x))) {
        m[i, ] <- order(.neighbours(d, i))[k]
    }
    m
}

#' Subsetting dist
#'
#' Return a column/all neighbours/competitors of an element of a `dist` without
#' turning the `dist` object into a `matrix` before.
#'
#' @param d `dist`
#' @param j `integer`, column index.
#' @return column/neighbour/competitor values.
#' @noRd
.neighbours <- function(d, j) {
    if (!inherits(d, "dist"))
        stop("'d' is not a 'dist' object.")
    if (length(j) != 1L || !is.integer(j))
        stop("'j' has to be an integer of length 1.")
    n <- attr(d, "Size")
    i <- seq_len(n)[-j]
    k <- ifelse(i < j,
        n * (i - 1L) - i * (i - 1L) / 2L +  j - i,
        (2 * n - j) * (j - 1) / 2 + i - j
    )
    d[k]
}

#' Calculate SMOTE duplicates
#'
#' Calculate SMOTE duplicates to upsample to majority class
#'
#' @param n `numeric(1)`, total number of samples.
#' @param nmin `numeric(1)`, number of minority samples.
#' @return `numeric(1)`, number of duplicates necessary to upsample to majority
#' class.
#' @noRd
.ndups <- function(n, nmin) {
    as.integer(floor((n - 2L * nmin) / nmin))
}

#' Find minority class
#'
#' @param y binary class vector.
#' @return minority class
#' @noRd
.minority <- function(y) {
    tbl <- table(y)
    r <- names(if (tbl[1] > tbl[2]) tbl[2] else tbl[1])
    mode(r) <- mode(y)
    r
}
