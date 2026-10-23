#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat arma_cosine(const arma::mat &mat){
  arma::mat cosines = mat * mat.t();
  arma::mat square = mat % mat;
  arma::colvec b = sum(square,1);
  arma::mat denum = sqrt(b) * sqrt(b.t());
  return cosines / denum;
  }


// [[Rcpp::export]]
double jaccard_cpp(NumericVector x, NumericVector y) {
  double sum_xy = 0, diff = 0;
  int n = x.size();
  for(int i = 0; i < n; ++i){
    double xi = x[i], yi = y[i]; 
    sum_xy += xi + yi;
    diff += std::abs(xi - yi);
  }
  return (sum_xy - diff) / (sum_xy + diff);
}

// [[Rcpp::export]]
NumericMatrix to_edge_list(NumericMatrix x) {
  int n = x.nrow(), ind = 0;
  NumericMatrix edge_list((n * (n-1)) / 2,3); 
  for(int i = 0; i < (n-1); ++i){
    for(int j = (i + 1); j < n; ++j){
      edge_list(ind,0) = i + 1;
      edge_list(ind,1) = j + 1;
      edge_list(ind,2) = x(i, j);
      ind++;
    }
  }
  return edge_list;
}


