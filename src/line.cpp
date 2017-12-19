#include "line.h"
#include <Rcpp.h>
using namespace Rcpp;

Line::Line() {
  f = 0;
  v = 0;
  c = 0;
  a = 0;
}


void Line::find_line(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical) {
  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  const int n = features.nrow();
  double old_m = 0, x = 0, y = 0, w = 0;
  double variance, covariance, x_mean, y_mean, w_total;
  double slope, intercept;
  double feature_sse = 0, sse = R_PosInf;
  std::vector<double> values;

  for (int j = 0; j < features.ncol(); j++) {
    feature_sse = 0;
    variance = 0;
    covariance = 0;
    values.clear();
    y_mean = 0;
    x_mean = 0;
    w_total = 0;
    if (categorical(j) == 1) {
      // categorical test
      y_mean = outcomes(ordered_index(0, j));
      for (int i = 1; i < n; i++) {
        if (features(ordered_index(i - 1, j), j) != features(ordered_index(i, j), j) ) {
          feature_sse = feature_sse + variance;
          values.push_back(y_mean);
          variance = 0;
          y_mean = 0;
          w_total = 0;
        }
        y = outcomes(ordered_index(i, j));
        w = weights(ordered_index(i, j));
        w_total += w;
        old_m = y_mean;
        y_mean = old_m + (w / w_total) * (y - old_m);
        variance = variance + w * (y - old_m) * (y - y_mean);
      }
      feature_sse = feature_sse + variance;
      values.push_back(y_mean);
      if (feature_sse < sse) {
        sse = feature_sse;
        f = j;
        c = 1;
        b = values;
      }
    } else {
      // continuous test
      for (int i = 0; i < n; i++) {
        x = features(i, j);
        y = outcomes(i);
        w = weights(i);
        w_total = w_total + w;
        old_m = x_mean;
        x_mean = old_m + (w / w_total) * (x - old_m);
        variance = variance + w * (x - old_m) * (x - x_mean);
        old_m = y_mean;
        y_mean = old_m + (w / w_total) * (y - old_m);
        covariance = covariance + weights(i) * (y - old_m) * (x - x_mean);
      }
      slope = covariance / variance;
      intercept = y_mean - slope * x_mean;
      for (int i = 0; i < n; i++) {
        feature_sse = feature_sse + weights(i) * pow(features(i, j) * slope + intercept - outcomes(i), 2);
      }
      if (feature_sse < sse) {
        sse = feature_sse;
        f = j;
        c = 0;
        a = slope;
        values.push_back(intercept);
        b = values;
      }
    }
  }
}

void Line::set_vote(double vote) {
  v = vote;
}


double Line::prediction(double x) const {
  if (c == 1) {
    return b[int(x)];
  }
  else {
    return a * x + b[0];
  }
}


int Line::feature() const {
  return f;
}


double Line::vote() const {
  return v;
}


NumericVector Line::vector() const {
  return NumericVector::create(f, v, a, b[0]);
}


