#ifndef LINE_H
#define LINE_H

#include <Rcpp.h>
using namespace Rcpp;

class Line
{
public:
  Line();

  void find_line(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical);
  void set_vote(double vote);

  double prediction(double x) const;
  int feature() const;
  double vote() const;
  NumericVector vector() const;

private:
  int f; // feature
  double v; // vote
  int c; // is_categorical (0 if false, 1 if true)
  double a; // slope if not categorical
  std::vector<double> b; // values if categorical, intercept if not

};


#endif
