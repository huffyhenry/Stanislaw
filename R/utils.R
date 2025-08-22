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


#' Optionally parallelised `lapply`.
#'
#' Excecute either `lapply` or `parallel::mclapply`, depending on the value
#' of a Boolean flag.
#'
#' @param X List or vector of arguments.
#' @param FUN Function to apply to these arguments.
#' @param parallel If FALSE (the default), call `lapply`. Otherwise call
#' `parallel::mclapply`.
#' @param ... Additional parameters passed to the wrapped function. See Details below.
#'
#' @details When `parallel` is TRUE and `...` does not specify `mc.cores`,
#' it is set to the number of available cores minus one, or to the
#' length of `X`, whichever is smaller.
#'
#' @returns The result of calling the wrapped function.
fapply <- function(X, FUN, parallel = FALSE, ...) {
  if (parallel) {
    if ("mc.cores" %in% ...names()) {
      parallel::mclapply(X, FUN, ...)
    } else {
      mc.cores <- min(parallel::detectCores() - 1, length(X))
      parallel::mclapply(X, FUN, mc.cores = mc.cores, ...)
    }
  } else {
    lapply(X, FUN, ...)
  }
}
