#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision stumps
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as a stump with a vote, and updates weights vector
// [[Rcpp::export]]
NumericVector adaboost(NumericMatrix features, NumericMatrix outcome_index, NumericVector outcomes, NumericVector &weights) {



  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  int index = 0;
  double error = 0, vote = 0, weight_sum = 0;
  NumericVector prediction(features.nrow());

  // feature, split, direction, vote
  Stump stump;
  NumericVector output(4);



  // FIND BEST DECISION STUMP
  // --------------------------------------------------------------------------------

  stump.find_stump(features, outcome_index, outcomes, weights);


  // PERFORM ADABOOST
  // --------------------------------------------------------------------------------

  // find prediction, error, and vote
  for (int i = 0; i < features.nrow(); i++) {
    index = outcome_index(i, stump.get_feature());
    if (features(i, stump.get_feature()) < stump.get_split()) {
      if (stump.get_direction() == 1) {
        prediction(index) = -1;
      } else {
        prediction(index) = 1;
      }
    } else {
      if (stump.get_direction() == 1) {
        prediction(index) = 1;
      } else {
        prediction(index) = -1;
      }
    }
    error = error + weights(index) * outcomes(index) * prediction(index);
  }
  error = .5 - .5 * error;
  vote = .5 * log((1 - error) / error);
  stump.set_vote(vote);

  // update weights
  for (int i = 0; i < weights.size(); i++) {
    weights(i) = weights(i) * exp(-1 * vote * prediction(i) * outcomes(i));
    weight_sum += weights(i);
  }
  for (int i = 0; i < weights.size(); i++) {
    weights(i) = weights(i) / weight_sum;
  }


  // CREATE CLASSIFIER OUTPUT
  // --------------------------------------------------------------------------------

  output(0) = stump.get_feature();
  output(1) = stump.get_split();
  output(2) = stump.get_direction();
  output(3) = stump.get_vote();

  return output;
}

