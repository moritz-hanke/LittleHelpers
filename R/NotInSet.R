#' Not In Set operator (opposite to \%in\%)
#'
#' @param x A vector.
#' @param y A vector.
#' @return A vector of length \code{x} with \code{TRUE}/\code{FALSE}.
#' @export
#' @examples
#' x <- c("a", 2, 3)
#' y <- c("b", 2, 3, 4)
#' x %!in% y

'%!in%' <- function(x,y){
  !('%in%'(x,y))
}
