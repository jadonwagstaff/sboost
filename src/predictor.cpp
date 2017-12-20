#include "predictor.h"
#include "line.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

Predictor::Predictor() {
  y = 0;
  vote_total = 0;
}

Predictor::Predictor(NumericVector features, double outcome) {
  x = features;
  y = outcome;
  vote_total = 0;
}

void Predictor::update_prediction(Line& line) {
  double prediction = line.prediction(x(line.feature()));
  bool inserted = false;

  for (int i = 0; i < p.size(); i++) {
    if (prediction < p(i)) {
      p.insert(p.begin() + i, prediction);
      v.insert(v.begin() + i, line.vote());
      inserted = true;
      break;
    }
  }

  if (inserted == false) {
    p.push_back(prediction);
    v.push_back(line.vote());
  }

  vote_total += line.vote();
}

double Predictor::outcome() const {
  return y;
}

double Predictor::prediction() const {
  double vote_sum = 0;
  for (int i = 0; i < p.size(); i++) {
    vote_sum += v(i);
    if (vote_sum >= .5 * vote_total) {
      return(p(i));
    }
  }
  return(0);
}



