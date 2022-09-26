#' Rank-normalize a vector
#'
#' Given vector x, return vector of ranks, min-max normalized
#'
#' @param x a numeric vector
#'
#' @return a vector with elements in range (0,1) proportional to ranks of x
#'
#' @examples
#' x = c(1,1,0,0,.5,.4)
#' rnorm(x)
#'
#' @export
#'
rank_norm <- function(x){
  rx <- rank(x)
  rx <- (rx-1)/(max(rx)-1)
  rx
}
