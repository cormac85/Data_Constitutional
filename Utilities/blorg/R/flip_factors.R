#' Flip Factor Order
#'
#' Takes a factor vector and reverses the order of the levels.
#' @author Cormac Nolan
#' @param x Factor vector which you would like to reverse the order of the factor levels.
#' @return Factor vector with levels reversed.
#' @export

flip_factors <- function(x){
  return(factor(x, levels = rev(levels(x))))
}
