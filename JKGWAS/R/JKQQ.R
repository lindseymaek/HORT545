#' QQ plot
#'
#'
#' @description Function to generate QQ-plot
#'
#' @param Pvals Input vector of Pvals such as an object returned by the JKGLM function
#'
#'
#' @return QQplot with user inputs.
#'
#' @export
#'
#'


JKQQ = function(Pvals){

  np=length(Pvals)
  set.seed(9);
  p.uni=runif(np,0,1);
  order.obs=order(Pvals)
  order.uni=order(p.uni)
  plot(-log10(p.uni[order.uni]),-log10(Pvals[order.obs]),
       main="QQPlot",
       xlab = "Expected",
       ylab = "Observed")
  abline(a = 0, b = 1, col = "#1DD3B0",lwd=2)
}


