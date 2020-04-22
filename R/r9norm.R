#' Decile normalization
#'
#' rank-normalize using 9 quantiles (just as ConSurfDB des to colour 3D structures) this is further smoothing
#'
#' @param x a numeric vector
#'
#' @return an integer vector of normalized values
#'
#' @examples
#'
#' x = rnorm(100)
#' r9norm(x)
#'
#' @export
#'
r9norm <- function(x){
  rx <- rank(x)
  rx <- (rx-1)/(max(rx)-1)
  rx <- as.integer(rx*9+1)
  rx[rx == 10] <- 9
  rx
}
