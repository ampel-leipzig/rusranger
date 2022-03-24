#' RUS ranger
#'
#' Adapted default settings to the [`ranger()`] to support
#' random-under-sampling. Additionally the default settings are modified to the
#' most common classification settings used in the AMPEL project.
#'
#' @details
#' In contrast to [`ranger()`] `rusranger()` currently just supports binary
#' classifications.
#'
#' @param x `matrix`/`data.frame`, feature matrix, see [`ranger()`] for
#' details.
#' @param y `numeric`/`factor`, classification labels, see [`ranger()`] for
#' details.
#' @param probability `logical(1)`, grow probability trees, see [`ranger()`]
#' for details.
#' @param classification `logical(1)`, run classification even if `y` is
#' `numeric`, see [`ranger()`] for details.
#' @param min.node.size, same as in [`ranger()`]
#' @param replace, subsampling without (default, `replace = FALSE`) or with
#' resampling, see [`ranger()`] for details.
#' @param \ldots further arguments passed to [`ranger()`].
#'
#' @return `ranger` object, see [`ranger()`] for details.
#'
#' @seealso
#' [`ranger()`]
#'
#' @references
#' *AMPEL* project:
#' Analysis and Reporting System for the Improvement of Patient Safety
#' through Real-Time Integration of Laboratory Findings,
#' https://ampel.care.
#'
#' @import ranger
#' @export
rusranger <- function(x, y, probability = TRUE, classification = !probability,
                      min.node.size = if (probability) 10 else 1,
                      replace = FALSE, ...) {
    ranger(
        x = as.data.frame(x), y = y,
        probability = probability,
        classification = classification,
        min.node.size = min.node.size,
        ## RUS
        ### subsample (replace == FALSE) vs bootstrap (replace == TRUE)
        replace = replace,
        ### case.weights for RUS are inverse proportions of the class for 50:50
        case.weights = .caseweights(y, replace = replace),
        ### sample.fraction is two times the minority class for 50:50 sampling
        sample.fraction = .samplefraction(y),
        ## END OF RUS
        ...,
        # set to TRUE for debugging inbag sampling
        keep.inbag = FALSE
    )
}
