#' Cross Validation for rusranger
#'
#' Runs a cross validation for [`rusranger()`].
#' The cross validation optimises the AUC.
#'
#' @inheritParams rusranger
#' @param nfolds `integer(1)` number of cross validation folds.
#' @param \ldots further arguments passed to [`rusranger()`].
#'
#' @return `double(1)` median AUC across all cross validation splits
#' @import future
#' @importFrom future.apply future_lapply
#' @importFrom ROCR performance prediction
#' @export
cv_rusranger <- function(x, y, nfolds = 5, ...) {
    folds <- .bfolds(y, nfolds = nfolds)
    xl <- split(x, folds)
    yl <- split(y, folds)
    auc <- unlist(future.apply::future_lapply(
        seq_len(nfolds),
        function(i) {
            xtrain <- do.call(rbind, xl[-i])
            xtest <- xl[[i]]
            ytrain <- do.call(c, yl[-i])
            ytest <- yl[[i]]
            rngr <- rusranger(x = xtrain, y = ytrain, ...)
            pred <- as.numeric(predict(rngr, xtest)$predictions[, 2L])
            performance(prediction(pred, ytest), measure = "auc")@y.values[[1L]]
        },
        future.seed = TRUE
    ))
    median(auc)
}

#' Repeated Cross Validation for rusranger
#'
#' Runs a repeated cross validation for [`rusranger()`].
#' The cross validation optimises the AUC.
#'
#' @inheritParams rusranger
#' @param nfolds `integer(1)` number of cross validation folds.
#' @param nrepcv `integer(1)` number of repeats.
#' @param \ldots further arguments passed to [`cv_rusranger()`].
#'
#' @return `double(5)`, minimal, 25 % quartiel, median, 75 % quartile and
#' maximal AUC across the repeated cross validations.
#' @importFrom stats median predict quantile setNames
#' @export
#' @examples
#' iris <- subset(iris, Species != "setosa")
#' rcv_rusranger(
#'     iris[-5], as.numeric(iris$Species == "versicolor"),
#'     nfolds = 3, nrepcv = 3
#' )
rcv_rusranger <- function(x, y, nfolds = 5, nrepcv = 2, ...) {
    auc <- unlist(future.apply::future_lapply(
        seq_len(nrepcv),
        function(i)cv_rusranger(x = x, y = y, nfolds = nfolds, ...),
        future.seed = TRUE
    ))
    setNames(
        quantile(auc, names = FALSE), c("Min", "Q1", "Median", "Q3", "Max")
    )
}
