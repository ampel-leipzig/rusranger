#' Cross Validation
#'
#' Runs a cross validation for train and prediction function.
#'
#' @inheritParams rusranger
#' @param FUN `function` function to optimize.
#' @param nfolds `integer(1)` number of cross validation folds.
#' @param \ldots further arguments passed to `FUN`.
#'
#' @return `double(1)` median AUC across all cross validation splits
#'
#' @note
#' The function to optimize has to accept five arguments: xtrain, ytrain, xtest,
#' ytest and \ldots.
#'
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom ROCR performance prediction
#' @export
#' @examples
#' .rusranger <- function(xtrain, ytrain, xtest, ytest, ...) {
#'     rngr <- rusranger(x = xtrain, y = ytrain, ...)
#'     pred <- as.numeric(predict(rngr, xtest)$predictions[, 2L])
#'     performance(prediction(pred, ytest), measure = "auc")@y.values[[1L]]
#' }
#' cv(iris[-5], as.numeric(iris$Species == "versicolor"), .rusranger, nfolds = 3)
cv <- function(x, y, FUN, nfolds = 5, ...) {
    folds <- .bfolds(y, nfolds = nfolds)
    xl <- split(x, folds)
    yl <- split(y, folds)
    r <- unlist(future.apply::future_lapply(
        seq_len(nfolds),
        function(i) {
            xtrain <- do.call(rbind, xl[-i])
            xtest <- xl[[i]]
            ytrain <- do.call(c, yl[-i])
            ytest <- yl[[i]]
            do.call(
                FUN,
                list(
                    xtrain = xtrain, ytrain = ytrain,
                    xtest = xtest, ytest = ytest,
                    ...
                )
            )
        },
        future.seed = TRUE
    ))
    median(r)
}

#' Repeated Cross Validation
#'
#' Runs a repeated cross validation for a function.
#' See also [`cv()`].
#'
#' @inheritParams cv
#' @param nrepcv `integer(1)` number of repeats.
#' @param \ldots further arguments passed to `FUN`.
#'
#' @return `double(5)`, minimal, 25 % quartiel, median, 75 % quartile and
#' maximal results across the repeated cross validations.
#' @importFrom stats median predict quantile setNames
#' @export
rcv <- function(x, y, nfolds = 5, nrepcv = 2, FUN, ...) {
    FUN <- match.fun(FUN)
    r <- unlist(future.apply::future_lapply(
        seq_len(nrepcv), function(i)
            cv(x = x, y = y, nfolds = nfolds, FUN = FUN, ...),
        future.seed = TRUE
    ))
    setNames(
        quantile(r, names = FALSE), c("Min", "Q1", "Median", "Q3", "Max")
    )
}
