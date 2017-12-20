#ifndef PREDICTOR_H
#define PREDICTOR_H

#include "line.h"
#include <Rcpp.h>
using namespace Rcpp;

class Predictor
{
public:
  Predictor();
  Predictor(NumericVector features, double outcome);

  void update_prediction(Line& line);

  double outcome() const;
  double prediction() const;

private:
  NumericVector x; // features
  NumericVector p; // predictions
  NumericVector v; // votes
  double y; // outcome
  double vote_total;

};


#endif
