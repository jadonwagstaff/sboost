#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision stumps
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as stumps with a vote
// [[Rcpp::export]]
List adaboost(const NumericMatrix& features, const NumericMatrix& ordered_index, const NumericVector& outcomes, const NumericVector& categorical, int iterations) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  Stump::populate_data(features, outcomes, ordered_index, categorical);
  NumericVector weights(outcomes.size());
  for (int i = 0; i < outcomes.size(); i++) {
    weights(i) = double(1) / outcomes.size();
  }

  double error = 0, vote = 0, weight_sum = 0;
  NumericVector predictions(features.nrow());

  std::vector<Stump> classifier(iterations);
  List output(iterations);

  for (int k = 0; k < iterations; k++) {

    // FIND BEST DECISION STUMP
    // --------------------------------------------------------------------------------
    classifier[k].find_stump(weights);


    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    classifier[k].update_predictions(predictions);
    for (int i = 0; i < features.nrow(); i++) {
      error = error + weights(i) * outcomes(i) * predictions(i);
    }
    error = .5 - .5 * error;
    vote = .5 * log((1 - error) / error);
    classifier[k].set_vote(vote);

    // update weights
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) * exp(-1 * vote * predictions(i) * outcomes(i));
      weight_sum += weights(i);
    }
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) / weight_sum;
    }
  }



  // CREATE CLASSIFIER OUTPUT
  // --------------------------------------------------------------------------------
  for (int i = 0; i < iterations; i++) {
    output[i] = classifier[i].make_vector();
  }

  return output;
}



