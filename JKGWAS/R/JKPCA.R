#'  Principle Component Analysis
#'
#'
#' @description Removes PCs that are linearly dependent with the given covariates and also user can specify how many PCs to chooose as co-factors
#'
#'
#' @param npc Number of Principle Components (PCs) that are specified by the user 
#' @param X     Markers data in the form n by m with n number of individuals and m number of markers
#' @param CV     Covariates matrix in the form n by t with n number of individuals and t number of co-variates
#'
#' @export
#' @return Principle Component Analysis
#' 

JKPCA = function(X,CV = NULL, npc = 5){
  X = dplyr::select_if(X, is.numeric);
  jkpca = prcomp(X);
  pc = jkpca$x;
  pc = pc[,c(1:npc)]; 
  pc.out = pc;
  
  if(!is.null(CV)){
    
  CV = dplyr::select_if(CV, is.numeric);
  JKmatrix = as.matrix(cbind(CV, pc));
  nc = ncol(CV)+1;
  rankByRemoved <- sapply(nc:ncol(JKmatrix), function (x) qr(JKmatrix[,-x])$rank);
  removeIDs = which(rankByRemoved == max(rankByRemoved));
  
  #dependent.free = mgcv::fixDependence(X1 = CV, X2 = pc, strict = FALSE,tol = );
  pc.out = pc[,-c(removeIDs)];
  
  }
  
  return(pc.out);
}

#https://stats.stackexchange.com/questions/16327/testing-for-linear-dependence-among-the-columns-of-a-matrix