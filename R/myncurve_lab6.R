#' @title Lab 6 Function - Normal curve
#'
#' @param mu the mean of the normal curve
#' @param sigma the sd of the normal curve
#' @param a an x value within the curve
#'
#' @return returns a plot of the normal curve that is shaded from effectively -infinity to a. Also returns the area of the shaded area to the command line.
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=0, sigma=3, a=1)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma))

  xcurve = seq(mu-5*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-5*sigma, xcurve, a), c(0, ycurve, 0), col=2)

  area = (pnorm(a, mu, sigma))
  area = round(area, 4)
  cat(area)
}
