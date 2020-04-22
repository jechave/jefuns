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
  z <- (x-min(x))/(max(x)-min(x))
  z
}
