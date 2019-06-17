#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat projectPairs2(arma::mat pairX, arma::mat Xdata){
  
 arma::mat pair_proj = pairX * arma::pinv(pairX.t() * pairX) * pairX.t();
 arma::mat temp = eye(size(pair_proj));
 arma::mat result = temp - pair_proj;
 arma::mat final_result = result * Xdata;
 
  return(final_result);
}

