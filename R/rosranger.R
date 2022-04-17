#' ROS ranger
#'
#' Adapted default settings to the [`ranger()`] to support
#' random-over-sampling. Additionally the default settings are modified to the
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
#' @param ndups `numeric(1)`, times of duplication of minority class.
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
rosranger <- function(x, y, probability = TRUE, classification = !probability,
                      min.node.size = if (probability) 10 else 1, ndups = 1,
                      ...) {
    ranger(
        x = as.data.frame(x), y = y,
        probability = probability,
        classification = classification,
        min.node.size = min.node.size,
        replace = TRUE,
        case.weights = .roscaseweights(y, ndups),
        ...,
        keep.inbag = FALSE
    )
}
