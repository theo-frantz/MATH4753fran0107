#' @title Lab 4 function
#'
#' @param x is an independent value
#'
#' @return y hat value
#' @export
#'
#' @examples
#' \dontrun{myquadplot(x = 11)}
myquadplot = function(x){
  quad.lm$coef[1] + quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}

