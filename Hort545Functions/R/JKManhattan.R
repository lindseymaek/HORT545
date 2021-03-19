#' Manhattan Plot for GWAS Visualization
#' 
#'  
#' @description Visualize the GWAS by GLM results by Manhattan plot. User can also specify QTN
#' 
#' 
#' @param Pvals Input vector of Pvals such as an object returned by the JKGLM function
#' @param SNP Input matrix containing SNP location. Must include columns Position and Chromosome
#' @param sig.cutoff Significance threshold for visualization. If NULL, then uses bonferroni correction with alpha = 0.05
#' @param QTN Vector of QTN positions that is provided by the user to highlight position in the Manhattan plot. If NULL, then QTN will not be identified.
#' 
#' @return Manhattan plot with user inputs.
#' @export
#' 
  
JKManhattan = function(Pvals, SNP, sig,cutoff = NULL, QTN = NULL){
  if(is.null(sig.cutoff)){
    sig.cutoff = 0.05/length(Pvals);
  }
  
  nChrom =length(unique(SNP$Chromosome))
  color.vector <- rep(c("#3c1642","#086375","#1DD3B0","#e01a4f"),nChrom)
  m=length(Pvals)
  plot(seq(1:m),t(-log10(Pvals)),
       col=color.vector[SNP[,2]],
       xlab="SNP Positions", 
       ylab = "-log(Pvalues)",
       main ="Manhattan Plot")
  abline(h=(-log10(sig.cutoff)), lwd=2, col = "black")
  if(!is.null(QTN)){
    points(QTN,-log10(Pvals[QTN]), pch=19)
  }
}



