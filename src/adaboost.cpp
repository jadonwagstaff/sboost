#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision stumps
// Param:
//   features - a matrix where each column is a feature and each row is an instance
//   ordered_index - a matrix the same size as features where each column is an
//     index of the corresponding feature column that, when used to index the feature,
//     puts the feature in numerical order.
//   outcomes - a vector where with two outcomes, 1 and -1
//   categorical - a vector with the same length as the number of colums in features,
//     1 is a categorical feature, 0 is a numeric feature
//   iterations - the number of stumps to create
// Return: list of stumps as vectors (see stump::make_vector() for structure)
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

  Stump classifier;
  List output(iterations);

  // BOOST OVER EACH ITERATION
  // --------------------------------------------------------------------------------
  for (int k = 0; k < iterations; k++) {

    // FIND BEST DECISION STUMP
    // --------------------------------------------------------------------------------
    classifier.find_stump(weights);


    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    classifier.new_predictions_integer(predictions);
    for (int i = 0; i < features.nrow(); i++) {
      error = error + weights(i) * outcomes(i) * predictions(i);
    }
    error = .5 - .5 * error;
    vote = .5 * log((1 - error) / error);
    classifier.set_vote(vote);

    // update weights
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) * exp(-1 * vote * predictions(i) * outcomes(i));
      weight_sum += weights(i);
    }
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) / weight_sum;
    }

    // create output
    output[k] = classifier.make_list();
  }

  return output;
}



