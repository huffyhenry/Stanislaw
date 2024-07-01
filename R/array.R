#' Extract and map array indices
#'
#' Extract indices of a posterior summary of an array of parameters to separate
#' columns, optionally mapping them to labels.
#'
#' @param df Data frame with a character column containing variable names
#' of the form \code{str[int,...,int]}, for example \code{phi[1,10,2]},
#' typically the output of [cmdstanr::summary] or [stansummary].
#' A single array summary is expected. Processing multiple arrays at once
#' will succeed only if all have the same dimension.
#' @param key Name of the column with array variable names. The default
#' corresponds to the output of [stansummary].
#' @param names Vector of strings to use as new column names.
#' Its length must match the array dimension.
#' When NULL, lower-case letters starting with "i" are used.
#' @param labels List of vectors of labels to use instead of array indices.
#' Length of the list must match array dimension. When NULL, no replacing takes
#' place. To replace along some dimensions but not others, pass the empty vector `c()`
#' at the correct position. See Examples below.
#'
#' @returns Data frame with additional columns appended.
#'
#' @examples
#' x <- data.frame(
#'   name = c("phi[1,1]", "phi[1,2]", "phi[2,1]", "phi[2,2]", "phi[3,1]", "phi[3,2]"),
#'   Mean = c(2830, 2833, 2790, 2791, 2700, 2730)
#' )
#' #       name Mean
#' # 1 phi[1,1] 2830
#' # 2 phi[1,2] 2833
#' # 3 phi[2,1] 2790
#' # 4 phi[2,2] 2791
#' # 5 phi[3,1] 2700
#' # 6 phi[3,2] 2730
#'
#' process_array_summary(x, names = c("player", "time"), labels = list(c("Carlsen", "Caruana", "Gukesh"), c()))
#' #       name Mean  player time
#' # 1 phi[1,1] 2830 Carlsen    1
#' # 2 phi[1,2] 2833 Carlsen    2
#' # 3 phi[2,1] 2790 Caruana    1
#' # 4 phi[2,2] 2791 Caruana    2
#' # 5 phi[3,1] 2700  Gukesh    1
#' # 6 phi[3,2] 2730  Gukesh    2
#'
#' @export
process_array_summary <- function(df,
                                  key = "name",
                                  names = NULL,
                                  labels = NULL) {

  #keys <- as.character(df[, key])
  keys <- df[[key]]
  keys <- gsub("\\]", "", gsub(".*\\[", "", keys))
  idxs <- strsplit(keys, ",")

  dims <- lengths(idxs)

  if (min(dims) != max(dims)) {
    stop("Varying number of array indices detected.")
  } else {
    n <- min(dims)
  }

  if (!is.null(names) & (length(names) != n)) {
    stop(paste0("Supplied vector of names does not match array dimension (", n, ")."))
  } else if (is.null(names)) {
    names <- letters[9:26][1:n]
  }

  if (!is.null(labels) & (length(labels) != n)) {
    stop(paste0("Supplied list of label sets does not match array dimension (", n, ")."))
  }

  idxs <- matrix(as.integer(unlist(idxs)), ncol = n, byrow = TRUE)

  for (i in 1:n) {
    if (is.null(labels) | (length(labels[[i]]) == 0)) {
      df[[names[i]]] <- idxs[, i]
    } else {
      df[[names[i]]] <- labels[[i]][idxs[, i]]
    }
  }

  df
}
