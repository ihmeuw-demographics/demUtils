#' @title The Inverse Gamma Distribution
#'
#' @description Random generation for the Inverse Gamma distribution with
#'   parameters `shape` (alpha) and `scale` (beta).
#'
#' @inheritParams stats::rgamma
#'
#' @return numeric vector of length \code{n}.
#'
#' @example
#' rinvgamma(10, 0.5, 0.01)
#'
#' @export
rinvgamma <- function(n, shape, scale) {
  return(1 / stats::rgamma(n, shape = shape, rate = scale))
}
