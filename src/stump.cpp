#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


NumericVector find_stump(NumericMatrix features, NumericMatrix outcome_index, NumericVector outcomes, NumericVector weights) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------

  // stump
  NumericVector output(3); // feature, split, direction, vote
  double positive_behind = 0, negative_behind = 0, positive_ahead = 0, negative_ahead = 0;
  double gain = 0, feature_gain = 0, feature_split = 0, feature_direction = 0;

  double best_feature = 0, best_split = 0, best_direction = 0, max_gain = 0;


  // STUMP
  // --------------------------------------------------------------------------------

  // go through each feature and find best gain
  for (int j = 0; j < features.ncol(); j++) {
    feature_gain = 0;
    // initialize counting variables
    positive_behind = 0;
    negative_behind = 0;
    positive_ahead = 0;
    negative_ahead = 0;
    for (int i = 0; i < features.nrow(); i++) {
      if (outcomes(outcome_index(i, j)) == 1) {
        positive_ahead += weights(outcome_index(i, j));
      } else {
        negative_ahead += weights(outcome_index(i, j));
      }
    }
    // find best gain for this features
    for (int i = 1; i < features.nrow(); i++) {
      // update counting variables
      if (outcomes(outcome_index(i - 1, j)) == 1) {
        positive_behind += weights(outcome_index(i - 1, j));
        positive_ahead += -1 * weights(outcome_index(i - 1, j));
      } else {
        negative_behind += weights(outcome_index(i - 1, j));
        negative_ahead += -1 * weights(outcome_index(i - 1, j));
      }

      // find gain if this and the last sample were different for this feature, compare to feature gain
      if (features(i - 1, j) != features(i, j) ) {

        if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
          gain = positive_ahead + negative_behind;
        } else {
          gain = negative_ahead + positive_behind;
        }


        if (gain > feature_gain) {
          feature_gain = gain;
          feature_split = (features(i - 1, j) + features(i, j)) / 2;
          if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
            feature_direction = 1;
          } else {
            feature_direction = -1;
          }
        }
      }
    }

    // if this features gain is the best, update maxGain and best Feature
    if (feature_gain > max_gain) {
      max_gain = feature_gain;
      best_feature = j;
      best_split = feature_split;
      best_direction = feature_direction;
    }

  }

  output(0) = best_feature;
  output(1) = best_split;
  output(2) = best_direction;
  return output;

}

