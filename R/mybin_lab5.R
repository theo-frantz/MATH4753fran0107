#' @title Lab 5 Function - Binomial
#'
#' @param iter number of iterations
#' @param n sample size in each iteration
#' @param p probablity of success
#' @param tab gives a table of successes if set to TRUE
#'
#' @return Returns a barplot of a series of binomial samples. Can also return a table.
#' @export
#'
#' @examples
#' \dontrun{mybin(iter=50, n=10, p=0.75, tab=TRUE)}
mybin = function(iter=100, n=10, p=0.5, tab=FALSE){
  #make an empty to hold the samples and fill it with NA to start
  sam.mat = matrix(NA, nr=n, nc=iter, byrow=TRUE)
  #Make a vector that will hold successes
  succ = c()

  for( i in 1:iter){
    #Fill each column in the matrix (sam.mat) with a new sample
    sam.mat[,i] = sample(c(1,0), n, replace=TRUE, prob=c(p, 1-p))
    #Find the number of successes in the sample and put that in succ
    succ[i] = sum(sam.mat[,i])
  }
  #make a table of successes
  succ.tab = table(factor(succ, levels=0:n))
  #make a barplot of the proportions of successes
  barplot(succ.tab/iter, col=rainbow(n+1), main="Binomial Simulation", xlab="Number of Successes")
  if(tab){
    succ.tab/iter
  }
}
