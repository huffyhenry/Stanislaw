

vgrepl <- function(patterns, x) {
  mask <- rep.int(FALSE, length(x))
  for (p in patterns) {
    mask <- mask | grepl(p, x)
  }

  mask
}

read_csv_header <- function(filepath, comment = '#') {
  con = file(filepath, 'r')
  line = readLines(con, n = 1)
  while (grepl(sprintf("%s.*", comment), line)) {
    line = readLines(con, n = 1)
  }
  close(con)
  unlist(strsplit(line, ','))
}

#'
#' @export
filter_cmdstan_csv <- function(src, pars, dst = tempfile()) {
  pars <- c(pars, "lp__")   # stansummary expects it and crashes when missing
  vars <- read_csv_header(src)
  mask <- vgrepl(pars, vars)
  indices <- seq(1:length(vars))[mask]

  if (length(indices) == 0) {
    warning("No parameters match the supplied patterns.")
    return(NULL)
  }
  else {
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

#' Summarise posterior draws.
#'
#' This function calculates a table of posterior summaries
#' similar to that from \link(`cmdstanr::summary`) but using CmdStan's
#' `stansummary` command-line utility instead of the `posterior` package.
#' This is much faster for very large models

#' @export
stansummary <- function(fit, pars = NULL, sig_figs = 4, lp__ = TRUE, parallel = TRUE) {
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

  if (!lp__) {
    df <- df[df$name != "lp__", ]
  }

  unlink(tmp)
  if (!is.null(pars)) {
    unlink(files)
  }

  df
}
