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
    cv(x = x, y = y, FUN = .rusranger, nfolds = nfolds, ...)
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
    rcv(x = x, y = y, nfolds = nfolds, nrepcv = nrepcv, FUN = .rusranger, ...)
}

#' Grid Search
#'
#' Grid search to optimise hyperparameters for `rusranger()`
#'
#' @inheritParams rusranger
#' @inheritParams rcv_rusranger
#' @param searchspace `data.frame`, hyperparameters to tune. Column names have
#' to match the argument names of [`ranger()`]/[`rusranger()`].
#' @param \ldots further arguments passed to [`rcv_rusranger()`].
#' @return `data.frame` with tested hyperparameters and AUCs
#' @export
#' @examples
#' iris <- subset(iris, Species != "setosa")
#' searchspace <- expand.grid(
#'    mtry = c(2, 3),
#'    num.trees = c(500, 1000)
#' )
#' ## nfolds and nrepcv are too low for real world applications, and are just
#' ## used for demonstration and to keep the run time of the examples low
#' gs_rusranger(
#'     iris[-5], as.numeric(iris$Species == "versicolor"),
#'     searchspace = searchspace, nfolds = 3, nrepcv = 1
#' )
gs_rusranger <- function(x, y, searchspace, nfolds = 5, nrepcv = 2, ...) {
    gridsearch(
        x = x, y = y, searchspace = searchspace,
        FUN = .rusranger, nfolds = nfolds, nrepcv = nrepcv, ...
    )
}

#' helper function to provide cv_rusranger/rcv_rusranger for backwards
#' compatibility
#'
#' @noRd
.rusranger <- function(xtrain, ytrain, xtest, ytest, ...) {
    rngr <- rusranger(x = xtrain, y = ytrain, ...)
    pred <- as.numeric(predict(rngr, xtest)$predictions[, 2L])
    performance(prediction(pred, ytest), measure = "auc")@y.values[[1L]]
}

#' Nested Cross Validation for Hyperparameter Search
#'
#' Run a grid search in a nested cross validation.
#'
#' @note
#' The reported performance could slightly differ from the median performance
#' in the reported gridsearch. After the gridsearch `rusranger` is trained again
#' with the best hyperparameters which results in a new subsampling.
#'
#' @inheritParams gs_rusranger
#' @param nouterfolds `integer(1)`, number of outer cross validation folds.
#' @param ninnerfolds `integer(1)`, number of inner cross validation folds.
#' @param nrepcv `integer(1)`, number repeats of inner cross validations.
#' @param \ldots further arguments passed to [`gs_rusranger()`].
#' @return `list`, with an element per `nouterfolds` containing the following
#' subelements:
#' * model selected `ranger` model.
#' * indextrain index of the used training items.
#' * indextest index of the used test items.
#' * prediction predictions results.
#' * truth original labels/classes.
#' * performance resulting performance (AUC).
#' * selectedparams select hyperparameters.
#' * gridsearch `data.frame`, results of the grid search.
#' * nouterfolds `integer(1)`.
#' * ninnerfolds `integer(1)`.
#' * nrepcv `integer(1)`.
#' @export
#' @examples
#' set.seed(20220324)
#' iris <- subset(iris, Species != "setosa")
#' searchspace <- expand.grid(
#'    mtry = c(2, 3),
#'    num.trees = c(500, 1000)
#' )
#' ## n(outer|inner) folds and nrepcv are too low for real world applications,
#' ## and are just used for demonstration and to keep the run time of the examples
#' ## low
#' nrcv_rusranger(
#'     iris[-5], as.numeric(iris$Species == "versicolor"),
#'     searchspace = searchspace, nouterfolds = 3, ninnerfolds = 3, nrepcv = 1
#' )
nrcv_rusranger <- function(x, y, searchspace,
                           nouterfolds = 5, ninnerfolds = 5, nrepcv = 2,
                           ...) {

    folds <- .bfolds(y, nfolds = nouterfolds)
    xl <- split(x, folds)
    yl <- split(y, folds)
    indices <- split(seq_along(y), folds)

    nrcv <- future.apply::future_lapply(
        seq_len(nouterfolds),
        function(i) {
            xtrain <- do.call(rbind, xl[-i])
            xtest <- xl[[i]]
            ytrain <- do.call(c, yl[-i])
            ytest <- yl[[i]]

            gs <- gs_rusranger(
                xtrain, ytrain, searchspace,
                nfolds = ninnerfolds, nrepcv = nrepcv, ...
            )

            top <- which.max(gs$Median)
            selparms <-
                gs[top,
                   !colnames(gs) %in% c("Min", "Q1", "Median", "Q3", "Max"),
                   drop = FALSE
                ]

            ## additional call of an already calculated tree, ...
            ## could be avoided if we would store the results of the trees
            ## but this would take alot of memory
            ## this could slightly change the results because of new
            ## resampling
            rngr <- do.call(
                rusranger,
                c(
                  list(
                    x = xtrain,
                    y = ytrain
                  ), list(...), selparms
                )
            )
            pred <- as.numeric(predict(rngr, xtest)$predictions[, 2L])

            list(
                model = rngr,
                indextrain = unlist(indices[-i]),
                indextest = unlist(indices[i]),
                prediction = pred,
                truth = ytest,
                performance = performance(
                    prediction(pred, ytest), measure = "auc"
                )@y.values[[1L]],
                selectedparams = selparms,
                gridsearch = gs,
                nouterfolds = nouterfolds,
                ninnerfolds = ninnerfolds,
                nrepcv = nrepcv
            )
        },
        future.seed = TRUE
    )
    nrcv
}
