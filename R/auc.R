#' Area Under the Curve
#'
#' Calculates AUC under a probability curve
#' @param p a numeric vector
#' @return a scalar, which is the AUC
#'
#' @examples
#' p <- c(1,3,5,7,8,2)
#' auc(p)
#'
#' @export
auc <- function(p){
    p <- p/sum(p)
    pcum <- p
    n <- length(p)
    for(n in seq(n)){
        pcum[n] <- sum(p[1:n])
    }
    auc <- sum(pcum)/n
    auc
}
