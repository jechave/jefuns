#' mean-normalization
#'
#' Normalizes elements of vector by dividing by mean
#' @param x a numeric vector
#' @return a vector of same length as input
#'
#' @examples
#' x = c(1,5,4)
#' mnorm(x)
#'
#' @export
mnorm <- function(x){
  z <- x/mean(x)
  z
}
