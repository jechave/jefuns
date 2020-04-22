#' Grishin's profile smoothing
#'
#' Smooths a profiles by averaging over windows, as implemented in AL2CO by Nick Grishin
#' @param x a numeric vector
#' @param window.half Integer size of the half window
#' @return a vector of same length as input
#'
#' @examples
#' x = c(1,5,4,3,3,7,8)
#' gsmooth(x)
#'
#' @export
gsmooth<-function(x,window.half=1){
# smooth using Grishin's method implemented in AL2CO
    sx <- x
    if(window.half !=0){
    x.mean <- mean(x)
    w <- 2*window.half+1
        for(i in (1:length(x)) ) {
            a <- max((i-window.half),1)
            b <- min((i+window.half),length(x))
            wp <- b-a+1

            sx[i] <- sum(x[a:b])/wp
            sx[i] <- x.mean + (sx[i] - x.mean)*sqrt(wp/w)
        }
    }
    return(sx)
}
