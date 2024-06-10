#' grepl vectorized over patterns
#'
#' Return a Boolean mask over `x` identifying elements that match at least one
#' of `patterns`.
#'
#' @param patterns Vector of regular expressions or strings.
#' @param x Character vector to scan for matches.
#'
#' @returns Boolean vector.
#'
#' @seealso [grepl].
vgrepl <- function(patterns, x) {
  mask <- rep.int(FALSE, length(x))
  for (p in patterns) {
    mask <- mask | grepl(p, x)
  }

  mask
}


#' Read CSV column names
#'
#' Read a CSV file in line by line, skipping comments until
#' the header is encountered.
#'
#' @param filepath Path to the CSV file.
#' @param comment Comment prefix.
#'
#' @returns The first non-commented line, presumably the header, as a character vector.
read_csv_header <- function(filepath, comment = '#') {
  con = file(filepath, 'r')
  line = readLines(con, n = 1)
  while (grepl(sprintf("%s.*", comment), line)) {
    line = readLines(con, n = 1)
  }
  close(con)
  unlist(strsplit(line, ','))
}


#' Discard unwanted CSV columns by streaming
#'
#' Forms and executes system commands resulting in
#' the `src` CSV being converted to `dst` by stripping comments and
#' keeping only the columns specified in `pars`.
#'
#' @param src Path to the CSV file to process.
#' @param pars Vector of regular expressions or strings identifying the columns
#' to keep.
#' @param dst Path to the processed CSV file.
#'
#' @returns `dst`.
filter_cmdstan_csv <- function(src, pars, dst = tempfile()) {
  pars <- c(pars, "lp__")   # stansummary expects it and crashes when missing
  vars <- read_csv_header(src)
  mask <- vgrepl(pars, vars)
  indices <- seq(1:length(vars))[mask]

  # Condense column indices into ranges to avoid any command length limits.
  fields <- c()
  N <- length(indices)
  curr <- 1
  while (curr <= N) {
    succ <- curr
    while ((succ + 1 <= N) && (indices[succ + 1] == indices[succ] + 1)) {
      succ <- succ + 1
    }
    if (curr == succ) {
      fields <- c(fields, as.character(indices[curr]))
    }
    else {
      fields <- c(fields, sprintf("%d-%d", indices[curr], indices[succ]))
    }
    curr <- succ + 1
  }

  # Simulate system pipe stripping comments and selecting fields
  #   grep -v "#" src | cut -d, -fx,y,z > dst
  tmp <- tempfile()
  system2(
    command = "grep",
    args = sprintf('-v "#" %s', src),
    stdout = tmp
  )
  system2(
    command = "cut",
    args = sprintf("-d, -f%s %s", paste(fields, collapse = ','), tmp),
    stdout = dst
  )
  unlink(tmp)

  dst
}


#' Slim down a collection of CSV files
#'
#' Apply `filter_cmdstan_csv` to a collection of CSV files,
#' optionally in parallel.
#'
#' @param src Vector of paths to the CSV files to be processed.
#' @param dst Vector of paths where the processed CSVs are to be written.
#' @param parallel Whether to parallelise the operation with [parallel::mclapply].
#' @inheritParams filter_cmdstan_csv
#'
#' @seealso [filter_cmdstan_csv]
#'
#' @returns `dst`
filter_cmdstan_csvs <- function(src,
                                pars,
                                dst = replicate(length(src), tempfile()),
                                parallel = TRUE) {
  if (parallel) {
    parallel::mclapply(
      1:length(src),
      function(i){filter_cmdstan_csv(src = src[i], pars = pars, dst = dst[i])},
      mc.cores = min(parallel::detectCores() - 1, length(src))
    )
  }
  else {
    lapply(
      1:length(src),
      function(i){filter_cmdstan_csv(src = src[i], pars = pars, dst = dst[i])}
    )
  }
}

#' Summarise posterior draws efficiently
#'
#' Calculate a table of posterior summaries similar to that
#' provided by [cmdstanr::summary] but using CmdStan's
#' `stansummary` command-line utility instead of the `posterior` package.
#' This is much faster for models with a very large number of parameters.
#' Additional time and memory savings can be achieved by subsetting parameters.
#'
#' @param fit A `CmdStanMCMC` or `CmdStanVB` object.
#' @param pars Character vector of regular expressions or strings
#' selecting the parameters to summarise.
#' See Details below. When NULL (default), all parameters are included.
#' @param sig_figs The number of significant figures to use in the output,
#' as interpreted by `stansummary`.
#' @param parallel Whether subsetting of CmdStan CSV files should be done
#' in parallel using [parallel::mclapply].
#'
#' @returns Data frame with posterior summary statistics for parameter of interest.
#'
#' @details
#' When called with `pars = NULL`, this function runs
#' `cmdstanr::cmdstan_path()/bin/stansummary` on `fit$output_files()` via
#' a \code{\link{system2}} call, writing the output to a temporary CSV file and
#' reading it back in.
#'
#' When `pars` is not `NULL`, the function scans the header of the CmdStan
#' CSVs for matches using \code{\link{grepl}}. Note that the indexed variables are
#' renamed in these CSVs so that "eta\[3,4\]" becomes "eta.3.4", and `pars`
#' has to be set accordingly.
#' Once the columns of interest are identified, slimmed-down temporary
#' CSVs with just the parameters of interest are passed to `stansummary`.
#' The pre-processing of CSVs is done by streaming them through `grep` and `cut`,
#' and can be done in parallel at a small memory cost.
#'
#' The parameter subsetting functionality requires sufficient disk space to store
#' temporary files, up to twice as much as taken by the CmdStan CSVs. These
#' files are cleared as soon as possible and in any event do not persist
#' beyond the lifetime of the R session that created them.
#'
#' This code has been tested on Linux only.
#'
#' @export
stansummary <- function(fit, pars = NULL, sig_figs = 3, parallel = TRUE) {
  files <- fit$output_files()
  if (!is.null(pars)) {
    files <- filter_cmdstan_csvs(src = files, pars = pars, parallel = parallel)
  }

  tmp <- tempfile()

  system2(
    command = sprintf("%s/bin/stansummary", cmdstanr::cmdstan_path()),
    args = sprintf("-s%d -c%s %s", sig_figs, tmp, paste(files, collapse = ' ')),
    stdout = NULL
  )
  df <- read.csv(file = tmp, comment.char = '#', check.names = FALSE)

  if (!is.null(pars) & !vgrepl(pars, "lp__")) {
    df <- df[df$name != "lp__", ]
  }

  unlink(tmp)
  if (!is.null(pars)) {
    unlink(files)
  }

  df
}
