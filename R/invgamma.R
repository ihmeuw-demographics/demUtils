#' @title The Inverse Gamma Distribution
#'
#' @description Random generation for the Inverse Gamma distribution with
#'   parameters `shape` (alpha) and `scale` (beta).
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'   the number required.
#' @param shape shape parameter of Inverse Gamma distribution, also known as the
#'   alpha parameter. Must be positive.
#' @param scale scale parameter of Inverse Gamma distribution, also known as the
#'   beta parameter. Must be positive.
#'
#' @return numeric vector of length \code{n}.
#'
#' @export
rinvgamma <- function(n, shape, scale) {
  return(1 / stats::rgamma(n, shape = shape, rate = scale))
}
