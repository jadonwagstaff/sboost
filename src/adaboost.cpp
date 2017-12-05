#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision classifier
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as a stump with a vote, and updates weights vector
// [[Rcpp::export]]
NumericMatrix adaboost(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, int iterations) {



  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  double error = 0, vote = 0, weight_sum = 0;
  NumericVector prediction(features.nrow());

  NumericVector weights(outcomes.size());
  for (int i = 0; i < outcomes.size(); i++) {
    weights(i) = double(1) / outcomes.size();
  }

  // feature, split, direction, vote
  std::vector<Stump> classifier(iterations);
  NumericMatrix output(iterations, 4);


  for (int k = 0; k < iterations; k++) {
  // FIND BEST DECISION STUMP
  // --------------------------------------------------------------------------------
    classifier[k].find_stump(features, ordered_index, outcomes, weights);



    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    for (int i = 0; i < features.nrow(); i++) {
      if (features(i, classifier[k].get_feature()) < classifier[k].get_split()) {
        if (classifier[k].get_direction() == 1) {
          prediction(i) = -1;
        } else {
          prediction(i) = 1;
        }
      } else {
        if (classifier[k].get_direction() == 1) {
          prediction(i) = 1;
        } else {
          prediction(i) = -1;
        }
      }
      error = error + weights(i) * outcomes(i) * prediction(i);
    }
    error = .5 - .5 * error;
    vote = .5 * log((1 - error) / error);
    classifier[k].set_vote(vote);

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
  for (int i = 0; i < iterations; i++) {
    output(i, 0) = classifier[i].get_feature();
    output(i, 1) = classifier[i].get_split();
    output(i, 2) = classifier[i].get_direction();
    output(i, 3) = classifier[i].get_vote();
  }

  return output;
}

