#include "stump.h"
#include <Rcpp.h>
using namespace Rcpp;

class Regression_Stump : public Stump
{
public:
  Regression_Stump();
  Regression_Stump(NumericVector stump_in);

  void find_stump(NumericMatrix& features, NumericMatrix& outcome_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical);

  double get_left() const;
  double get_right() const;
  NumericVector make_vector() const;

private:
  double left;
  double right;

};
