#' Extract subset of posterior draws
#'
#' Load draws of a subset of parameters from filtered CmdStan output files.
#' This tends to be more time and memory efficient than calling `$draws()`
#' on the associated fit object.
#'
#' @param fit Fitted Stan model (a `CmdStanMCMC` object.)
#' @param pars Character vector of regular expressions capturing the parameters
#' of interest. See Details below.
#' @param parallel Whether to scan files in parallel using [parallel::mclapply].
#' @param as_draws_df Whether to convert the result to [posterior::draws_df] format.
#' Requires that the `posterior` package is installed.
#'
#' @seealso [stansummary].
#'
#' @returns Data frame with one row per draw and one column per
#' parameter. If `as_draws_df` is TRUE, this is then passed to
#' [posterior::as_draws_df], yielding a draws object compatible with
#' `posterior` and related packages.
#'
#' @details
#' Indexed parameters are renamed in the CSV files, with the opening bracket and
#' commas replaced by full stops and the closing bracket dropped. For example,
#' to include draws of "eta\[3,4\]", `pars` needs to capture "eta.3.4" instead.
#'
#' @export
draws <- function(fit, pars, as_draws_df = FALSE, parallel = TRUE) {
  files <- filter_cmdstan_csvs(fit$output_files(), pars = pars, parallel = parallel)
  on.exit(unlink(files))

  read_tmp_csv <- function(filepath) {
    dr <- read.csv(file = filepath, comment.char = "#", check.names = FALSE)
    if (as_draws_df) {
      dr$.chain <- match(filepath, files)
      dr$.iteration <- 1:nrow(dr)
    }

    dr
  }

  maybe_as_draws_df <- function(dr) {
    if (as_draws_df)
      posterior::as_draws_df(dr)
    else
      dr
  }

  files |>
    fapply(FUN = read_tmp_csv, parallel = parallel) |>
    do.call(rbind, args = _) |>
    maybe_as_draws_df()
}
