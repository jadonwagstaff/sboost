#ifndef ASSESSMENT_H
#define ASSESSMENT_H

#include "stump.h"
#include <Rcpp.h>
using namespace Rcpp;

class Assessment
{
public:
  Assessment(int size);

  void update_predictions(Stump& classifier, NumericMatrix& features, NumericVector& outcomes);
  void update_contingency(NumericMatrix& features, NumericVector& outcomes);

  int get_true_positive() const;
  int get_false_negative() const;
  int get_true_negative() const;
  int get_false_positive() const;

private:
  NumericVector predictions;
  int true_positive;
  int false_negative;
  int true_negative;
  int false_positive;

};


#endif
