#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param x0 Initial value
//' @param n A given value
//' @param a A given value
//' @param b A given value
//' @return a random sample of size \code{n} 
//' @examples
//' \dontrun{
//' Y<-GibbsC(N, x0, n, a, b)
//' plot(Y[1001:N,1],Y[1001:N,2],xlab = "y1",ylab = "y2")
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix GibbsC(int N, NumericVector x0, int n, double a, double b) {
  NumericMatrix X(N,2);
  X(0, 0) = x0[1];
  X(0, 1) = x0[2];
  for(int i = 1; i < N; i++)
  {
    double x2 = X(i-1, 1);
    X(i, 0) = rbinom(1, n, x2)[0];
    double x1 = X(i, 0);
    X(i, 1) = rbeta(1, x1+a, n-x1+b)[0];
  }
  return X;
}
