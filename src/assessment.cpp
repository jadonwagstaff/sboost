#include "assessment.h"
#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

Assessment::Assessment(NumericMatrix features_in, NumericVector outcomes_in) {
  NumericVector predictions_in(features_in.nrow());

  features = features_in;
  outcomes = outcomes_in;
  predictions = predictions_in;
  true_positive = 0;
  false_negative = 0;
  true_negative = 0;
  false_positive = 0;
}

void Assessment::update_predictions(Stump classifier) {
  for (int i = 0; i < features.nrow(); i++) {
    if (features(i, classifier.get_feature()) < classifier.get_split()) {
      if (classifier.get_direction() == 1) {
        predictions(i) += -1 * classifier.get_vote();
      } else {
        predictions(i) += classifier.get_vote();
      }
    } else {
      if (classifier.get_direction() == 1) {
        predictions(i) += classifier.get_vote();
      } else {
        predictions(i) += -1 * classifier.get_vote();
      }
    }
  }
}

void Assessment::update_contingency() {
  true_positive = 0;
  false_negative = 0;
  true_negative = 0;
  false_positive = 0;
  for (int i = 0; i < features.nrow(); i++) {
    if (outcomes(i) == 1) {
      if (predictions(i) >= 0) {
        true_positive++;
      } else {
        false_negative++;
      }
    } else {
      if (predictions(i) < 0) {
        true_negative++;
      } else {
        false_positive++;
      }
    }
  }
}

int Assessment::get_true_positive() const {
  return true_positive;
}

int Assessment::get_false_negative() const {
  return false_negative;
}

int Assessment::get_true_negative() const {
  return true_negative;
}

int Assessment::get_false_positive() const {
  return false_positive;
}

