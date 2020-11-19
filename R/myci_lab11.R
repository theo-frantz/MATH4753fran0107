#' @title Lab 11 Function - Confidence Interval for the Mean
#'
#' @param x the sample data
#'
#' @return returns a 95 percent confidence interval for the population mean
#' @export
myci = function(x){

  #set up
  ybar = mean(x)
  s = sd(x)
  n = length(x)
  t = qt(1-(0.05/2), n-1)

  #calculate
  upper = ybar + t*(s/sqrt(n))
  lower = ybar - t*(s/sqrt(n))

  ci = c(lower, upper)
  return(ci)
}
