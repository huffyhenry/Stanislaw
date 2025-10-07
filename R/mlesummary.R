#' Extract estimates from MLE fit
#'
#' This function reads in parameter estimates from a `CmdStanMLE` object,
#' mirroring the output of the `$summary()` function in `cmdstanr`.
#'
#' @param fit CmdStan model fitted by penalised maximum likelihood (a `CmdStanMLE` object.)
#' @param pars Character vector of regular expressions selecting
#' the parameters to report. See the Details section of \link{stansummary} for
#' an important note on how to capture elements of vector and array parameters.
#'
#' @returns Data frame with `variable` and `estimate` columns.
#'
#' @seealso [stansummary()].
#'
#' @export
mlesummary <- function(fit, pars = NULL) {
  if (!inherits(fit, "CmdStanMLE")) {
    stop("This method can be used only with models fitted by optimisation.")
  }

  wide <- read.csv(fit$output_files(), comment.char = '#')

  long <- data.frame(
    variable = colnames(wide),
    estimate = unlist(wide[1, ]),
    row.names = NULL
  )

  if (!is.null(pars)) {
    long <- long[vgrepl(pars, long$variable), ]
  }

  long$variable <- repair_dotnames(long$variable)

  # Add same class labels as cmdstanr's $summary()
  class(long) <- c("draws_summary", "tbl_df", "tbl", "data.frame")

  long
}
