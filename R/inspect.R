#' Quick look at a CmdStan fit
#'
#' This function provides basic statistics about a CmdStan fit such as
#' the count of divergences and the average treedepth. The intended use
#' is to help the user decide whether to abort a fit that is in progress.
#'
#' @param cmdstan_csv_dir Directory to which CmdStan wrote or is writing the samples.
#' @param file_regex Regular expression to filter file names in `cmdstan_csv_dir`.
#' Useful if multiple fits are being stored in the same directory. The default
#' expression matches any file name.
#' @param iter_ignore Number of initial iterations to discard
#' before calculating summary statistics. Useful for skipping warmup iterations.
#' @param parallel Whether to process CmdStan CSV files in parallel with [parallel::mclapply].
#' @param print Whether to print the statistics on screen.
#'
#' @note
#' - The chain number reported may not match the number assigned by CmdStan.
#' - The iteration number reported is the number of samples found
#' in the file minus `iter_ignore`. When `iter_ignore` is 0 and CmdStan does not
#' save warmup samples, it is the number of completed sampling iterations.
#'
#' @returns A data frame of statistics, invisibly.
#'
#' @export
inspect_cmdstan_fit <- function(cmdstan_csv_dir,
                                file_regex = ".*",
                                iter_ignore = 0,
                                parallel = TRUE,
                                print = TRUE) {
  if (!dir.exists(cmdstan_csv_dir)) {
    stop(sprintf("Directory '%s' does not exist.", cmdstan_csv_dir))
  }

  csvs <- sort(list.files(path = cmdstan_csv_dir, full.names = FALSE))

  if (length(csvs) == 0) {
    stop(sprintf("Directory '%' is empty.", cmdstan_csv_dir))
  }

  csvs <- csvs[grepl(file_regex, csvs)]

  if (length(csvs) == 0) {
    stop("No files match the supplied regular expression.")
  }

  csvs <- sapply(csvs, function(csv) {file.path(cmdstan_csv_dir, csv)}, USE.NAMES = FALSE) |>
    filter_cmdstan_csvs(src = _, pars = c("lp__", "divergent__", "treedepth__"), parallel = parallel) |>
    unlist()

  result <- lapply(
    1:length(csvs),
    function(idx) {
      dr <- read.csv(csvs[idx], comment.char = '#')
      dr <- dr[(iter_ignore + 1):nrow(dr),]

      data.frame(
        chain = idx,
        curr_iter = nrow(dr),
        avg_lp = mean(dr$lp__),
        total_div = sum(dr$divergent__),
        avg_treedepth = mean(dr$treedepth__)
      )
    }
  ) |>
    do.call(rbind, args = _)

  if (print) {
    base::print(result)
  }

  invisible(result)
}
