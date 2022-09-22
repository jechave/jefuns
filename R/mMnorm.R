#' min-max normalization
#'
#' Scale vector so that range is [0,1]
#'
#' @param x a numeric vector
#'
#' @return a numeric vector (normalized)
#'
#' @examples
#' x = c(1,1,0,0,.5,.8)
#' mMnorm(x)
#'
#' @export
mMnorm <- function(x){
  range <- max(x) - min(x)
  if (dplyr::near(range, 0)) range <- 1
  z <- (x-min(x)) / range
  z
}
