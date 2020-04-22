#' Off-diagonal part of a matrix
#'
#' Given a matrix A, returns the a matrix with off-diagonal elements and 0's in diagonal
#'
#' @param A a square matrix
#'
#' @return a square off-diagonal matrix
#'
#' @examples
#' A = matrix(c(1,2,3,4), 2, 2)
#' offdiag(A)
#'
#' @export
offdiag <- function(A){
    n <- nrow(A)
    m <- ncol(A)
    if(n != m) print("ERROR in offdiag: non-square matrix")
    stopifnot(n == m)
    off.A <- A - diag(diag(A),n,n)
    off.A
}
