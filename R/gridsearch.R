#' Grid Search
#'
#' Grid search to optimise hyperparameters for `FUN`
#'
#' @inheritParams cv
#' @inheritParams rcv
#' @param searchspace `data.frame`, hyperparameters to tune. Column names have
#' to match the argument names of `FUN`.
#' @param \ldots further arguments passed to `FUN`
#' @return `data.frame` with tested hyperparameters and metric
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
gridsearch <- function(x, y, searchspace, FUN, nfolds = 5, nrepcv = 2, ...) {
    r <- future.apply::future_lapply(
        seq_len(nrow(searchspace)),
        function(i) {
            do.call(
                rcv,
                c(
                    list(
                        x = x, y = y, FUN = FUN,
                        nfolds = nfolds, nrepcv = nrepcv
                    ),
                    list(...),
                    searchspace[i, ]
                )
            )
        },
        future.seed = TRUE
    )
    cbind.data.frame(searchspace, do.call(rbind, r))
}

#' Nested Cross Validation for Hyperparameter Search
#'
#' Run a grid search in a nested cross validation.
#'
#' @note
#' The reported performance could slightly differ from the median performance
#' in the reported gridsearch. After the gridsearch `FUN` is trained again
#' with the best hyperparameters which results in a new subsampling.
#'
#' @inheritParams cv
#' @inheritParams rcv
#' @inheritParams gridsearch
#' @param nouterfolds `integer(1)`, number of outer cross validation folds.
#' @param ninnerfolds `integer(1)`, number of inner cross validation folds.
#' @param nrepcv `integer(1)`, number repeats of inner cross validations.
#' @param \ldots further arguments passed to [`gs_rusranger()`].
#' @return `list`, with an element per `nouterfolds` containing the following
#' subelements:
#' * indextrain index of the used training items.
#' * indextest index of the used test items.
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
nested_gridsearch <- function(x, y, searchspace, FUN,
                              nouterfolds = 5, ninnerfolds = 5, nrepcv = 2,
                              ...) {
    folds <- .bfolds(y, nfolds = nouterfolds)
    xl <- split(x, folds)
    yl <- split(y, folds)
    indices <- split(seq_along(y), folds)

    future.apply::future_lapply(
        seq_len(nouterfolds),
        function(i) {
            xtrain <- do.call(rbind, xl[-i])
            xtest <- xl[[i]]
            ytrain <- do.call(c, yl[-i])
            ytest <- yl[[i]]

            g <- gridsearch(
                x = xtrain, y = ytrain, searchspace = searchspace,
                FUN = FUN, nfolds = ninnerfolds, nrepcv = nrepcv, ...
            )

            top <- which.max(g$Median)
            selparms <-
                g[top,
                   !colnames(g) %in% c("Min", "Q1", "Median", "Q3", "Max"),
                   drop = FALSE
                ]

            r <- do.call(
                FUN,
                c(
                    list(
                        xtrain = xtrain, ytrain = ytrain,
                        xtest = xtest, ytest = ytest,
                        ...
                    ), selparms
                )
            )

            list(
                indextrain = unlist(indices[-i]),
                indextest = unlist(indices[i]),
                performance = r,
                selectedparams = selparms,
                gridsearch = g,
                nouterfolds = nouterfolds,
                ninnerfolds = ninnerfolds,
                nrepcv = nrepcv
            )
        },
        future.seed = TRUE
    )
}
