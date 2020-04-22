#' Effective number of states
#'
#' GIven a distribution, it calculates the effective number of states (exp(entropy))
#'
#' @param p a numeric vector, not necessarily sum(p) = 1 (but will be renormalized as a probability  vector)
#'
#' @return a scalar representing effective number of states that contribute to p
#'
#' @examples
#' p = c(1,1,0,0)
#' Kn(p)
#'
#' @export
Kn <- function(p){
    n <- length(p)
    p <- p/sum(p)
    s <- -sum(p*log(p))
    Kn <- exp(s)
    (Kn-1)/(n-1)
}
