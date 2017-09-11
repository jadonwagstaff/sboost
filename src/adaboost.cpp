#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision stumps
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as a stump with a vote, and updates weights vector
// [[Rcpp::export]]
NumericMatrix adaboost(NumericMatrix features, NumericMatrix outcome_index, NumericVector outcomes, NumericVector &weights, int iterations) {



  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  int index = 0;
  double error = 0, vote = 0, weight_sum = 0;
  NumericVector prediction(features.nrow());

  // feature, split, direction, vote
  std::vector<Stump> stumps(iterations);
  NumericMatrix output(iterations, 4);


  for (int k = 0; k < iterations; k++) {
  // FIND BEST DECISION STUMP
  // --------------------------------------------------------------------------------
    stumps[k].find_stump(features, outcome_index, outcomes, weights);



    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    for (int i = 0; i < features.nrow(); i++) {
      index = outcome_index(i, stumps[k].get_feature());
      if (features(i, stumps[k].get_feature()) < stumps[k].get_split()) {
        if (stumps[k].get_direction() == 1) {
          prediction(index) = -1;
        } else {
          prediction(index) = 1;
        }
      } else {
        if (stumps[k].get_direction() == 1) {
          prediction(index) = 1;
        } else {
          prediction(index) = -1;
        }
      }
      error = error + weights(index) * outcomes(index) * prediction(index);
    }
    error = .5 - .5 * error;
    vote = .5 * log((1 - error) / error);
    stumps[k].set_vote(vote);

    // update weights
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) * exp(-1 * vote * prediction(i) * outcomes(i));
      weight_sum += weights(i);
    }
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) / weight_sum;
    }

  }


  // CREATE CLASSIFIER OUTPUT
  // --------------------------------------------------------------------------------
  for (int i; i < iterations; i++) {
    output(i, 0) = stumps[i].get_feature();
    output(i, 1) = stumps[i].get_split();
    output(i, 2) = stumps[i].get_direction();
    output(i, 3) = stumps[i].get_vote();
  }

  return output;
}

