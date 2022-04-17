#' Create balanced CV folds
#'
#' taken from ampel-leipzig/ameld/R/utils-glmnet.R
#'
#' @param y `factor`, classes
#' @param nfolds `integer(1)`, number of folds
#' @return `integer(length(y))`, with fold id
#' @noRd
.bfolds <- function(y, nfolds = 3L) {
    grpn <- table(y)

    if (nfolds < 3L)
        stop("'nfolds' has to be >= 3.")
    if (any(nfolds > grpn))
        warning("'nfolds' > than the groups, reducing to minimal group size.")

    nfolds <- min(nfolds, grpn)
    s <- seq_len(nfolds)
    unlist(lapply(grpn, function(n)sample(rep_len(s, n))), use.names = FALSE)
}

#' Case weights
#'
#' Calculate case weights for 50:50 selection
#'
#' @param y `factor`/`numeric`, classes
#' @param replace `logical(1)`, sampling with replacement (bootstrap)?
#' @return `double(length(y))` with case weights
#' @noRd
.caseweights <- function(y, replace = FALSE) {
    y <- as.factor(y)
    if (isTRUE(replace))
        ## these case weights work just for replace = TRUE (=> bootstrap)
        c(1 / table(y))[y]
    else {
        ## if we don't using bootstrap we have to ensure that the cases
        ## (minority) class is selected, so we choose an arbitary high weight
        tbl <- table(y)
        r <- if (tbl[1] > tbl[2]) c(1, 1e3) else c(1e3, 1)
        names(r) <- names(tbl)
        r[y]
    }
}

#' N minority
#'
#' Find number of minority class samples.
#'
#' @param y vector of binary class labels
#' @return `integer(1)`, number of minority class samples
#' @noRd
.nmin <- function(y) {
    tbl <- table(y)
    as.vector(if (tbl[1] > tbl[2]) tbl[2] else tbl[1])
}

#' Sample fraction
#'
#' Calculate sample fraction for 50:50 selection (two times the minority class)
#'
#' @param y `factor`/`numeric`, classes
#' @return `double(1)`, sample fraction
#' @noRd
.samplefraction <- function(y) {
    2L * .nmin(y) / length(y)
}
