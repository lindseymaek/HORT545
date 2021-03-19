
library(tidyverse)
JKPCA = function(X,CV, npc = 5){
  X = dplyr::select_if(X, is.numeric);
  CV = dplyr::select_if(CV, is.numeric);
  jkpca = prcomp(X);
  pc = jkpca$x;
  pc = pc[,c(1:npc)];
  
  JKmatrix = as.matrix(cbind(CV, pc));
  nc = ncol(CV)+1;
  rankByRemoved <- sapply(nc:ncol(JKmatrix), function (x) qr(JKmatrix[,-x])$rank);
  removeIDs = which(rankByRemoved == max(rankByRemoved));
  
  #dependent.free = mgcv::fixDependence(X1 = CV, X2 = pc, strict = FALSE,tol = );
  pc.out = pc[,-c(removeIDs)];
  
  return(pc.out);
}

#https://stats.stackexchange.com/questions/16327/testing-for-linear-dependence-among-the-columns-of-a-matrix