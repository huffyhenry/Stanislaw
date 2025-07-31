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
#' @seealso [extract_matrix].
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

#' Convert a summary of draws to a matrix
#'
#' Arrange a posterior summary of parameters that are entries of a matrix
#' or of a 2D array into an R matrix object.
#'
#' @param df Data frame with posterior summaries of matrix entries. Typically
#' a result of asking for a [stansummary] or [cmdstanr::summary] of a matrix
#' parameter. Should not contain summaries of any other parameters.
#' @param key The column in `df` with the parameter name and indexes.
#' The default corresponds to the output format of [stansummary].
#' @param value The column in `df` from which the values are to be read into
#' the matrix. The default corresponds to the posterior mean as reported by [stansummary].
#' @param cnames Column names to be applied to the resulting matrix. Needs to
#' be a character vector of the length corresponding to the number of columns
#' in the matrix. If this condition is not met or if the argument is NULL
#' (the default), generic X1,...,Xn names are used.
#'
#' @returns A matrix.
#'
#' @seealso [process_array_summary()].
#'
#' @export
extract_matrix <- function(df,
                           key = "name",
                           value = "Mean",
                           cnames = NULL) {

  M <- process_array_summary(df = df, key = key, names = c(".row", ".col")) |>
    subset(select = c(".row", ".col", value)) |>
    reshape(direction = "wide", idvar = ".row", timevar = ".col", v.names = value) |>
    subset(select = -.row) |>
    as.matrix()

  if (is.null(cnames)) {
    cnames <- sprintf("X%d", 1:ncol(M))
  }
  else if (length(cnames) != ncol(M)) {
    warning(message = "Supplied vector of column names does not match matrix width. Generic names used instead.")
    cnames <- sprintf("X%d", 1:ncol(M))
  }

  colnames(M) <- cnames
  rownames(M) <- 1:nrow(M)

  M
}
