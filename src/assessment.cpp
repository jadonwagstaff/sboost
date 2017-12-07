#include "assessment.h"
#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

Assessment::Assessment(int size) {
  NumericVector predictions_in(size);
  predictions = predictions_in;
  true_positive = 0;
  false_negative = 0;
  true_negative = 0;
  false_positive = 0;
}

void Assessment::update_predictions(Stump& stump, NumericMatrix& features, NumericVector& outcomes) {
  if (stump.get_categorical() == 1) {
    for (int i = 0; i < features.nrow(); i++) {
      predictions(i) += -1 * stump.get_vote();
      for (int k = 0; k < stump.split_size(); k++) {
        Rcout << stump.get_split(k);
        if (features(i, stump.get_feature()) == stump.get_split(k)) {
          predictions(i) += 2 * stump.get_vote();
          break;
        }
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      if (features(i, stump.get_feature()) < stump.get_split()) {
        if (stump.get_direction() == 1) {
          predictions(i) += -1 * stump.get_vote();
        } else {
          predictions(i) += stump.get_vote();
        }
      } else {
        if (stump.get_direction() == 1) {
          predictions(i) += stump.get_vote();
        } else {
          predictions(i) += -1 * stump.get_vote();
        }
      }
    }
  }
  Rcout << "\n";
}

void Assessment::update_contingency(NumericMatrix& features, NumericVector& outcomes) {
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

