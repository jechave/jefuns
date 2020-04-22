#' z-score normalization
#'
#' Normalize z = (x - mean(x)) / sd(x)
#'
#' @param x a numeric vector
#'
#' @return a vector of z-scores
#'
#' @examples
#' x = rnorm(100)
#' znorm(x)
#'
#' @importFrom stats sd
#' @export
znorm <- function(x){
  z <- (x - mean(x))/sd(x)
  z
}
