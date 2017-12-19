#include "stump.h"
#include "line.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// adaboost is the central function for adaptive boosting of decision classifier
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as stumps with a vote
// [[Rcpp::export]]
List adaboost(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& categorical, int iterations) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  double error = 0, vote = 0, weight_sum = 0, value = 0;
  NumericVector prediction(features.nrow());
  int index = 0;
  bool positive = false;

  NumericVector weights(outcomes.size());
  for (int i = 0; i < outcomes.size(); i++) {
    weights(i) = double(1) / outcomes.size();
  }

  // feature, split, direction, vote
  std::vector<Stump> classifier(iterations);
  List output(iterations);


  for (int k = 0; k < iterations; k++) {
  // FIND BEST DECISION STUMP
  // --------------------------------------------------------------------------------
    classifier[k].find_stump(features, ordered_index, outcomes, weights, categorical);


    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    if (classifier[k].get_categorical() == 0) {
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
    } else {
      value = 0; // should not be a value in this feature
      for (int i = 0; i < features.nrow(); i++) {

        index = classifier[k].get_feature();

        if (features(ordered_index(i, index), index) != value) {
          value = features(ordered_index(i, index), index);
          positive = false;
          for (int j = 0; j < classifier[k].split_size(); j++) {
            if (value == classifier[k].get_split(j)) {
              positive = true;
              break;
            }
          }
        }

        if (positive == true) {
          prediction(ordered_index(i, index)) = 1;
        } else {
          prediction(ordered_index(i, index)) = -1;
        }

        error = error + weights(ordered_index(i, index)) * outcomes(ordered_index(i, index)) * prediction(ordered_index(i, index));
      }
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
    output[i] = classifier[i].make_vector();
  }

  return output;
}



// adaboost_regression is the central function for adaptive boosting of regression classifer
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as stumps with a vote
// [[Rcpp::export]]
List adaboost_regression(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& categorical, int iterations) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------
  double error = 0, vote = 0, weight_sum = 0, max_error = 0;
  NumericVector prediction(features.nrow());

  NumericVector weights(outcomes.size());
  for (int i = 0; i < outcomes.size(); i++) {
    weights(i) = double(1) / outcomes.size();
  }

  // feature, split, direction, vote
  std::vector<Line> classifier(iterations);
  List output(iterations);



  std::vector<double> p;
  double votesum = 0, votetotal = 0;
  int count;


  for (int k = 0; k < iterations; k++) {
    // FIND BEST DECISION STUMP
    // --------------------------------------------------------------------------------
    classifier[k].find_line(features, ordered_index, outcomes, weights, categorical);


    // PERFORM ADABOOST
    // --------------------------------------------------------------------------------

    // find prediction, error, and vote
    error = 0;
    weight_sum = 0;
    max_error = 0;
    for (int i = 0; i < features.nrow(); i++) {
      prediction(i) = classifier[k].prediction(features(i, classifier[k].feature()));
      if (std::abs(outcomes(i) - prediction(i)) > max_error) {
        max_error = std::abs(outcomes(i) - prediction(i));
      }
    }
    max_error = pow(max_error, 2);
    for (int i = 0; i < features.nrow(); i++) {
      prediction(i) = pow(outcomes(i) - prediction(i), 2) / max_error;
      error = error + weights(i) * prediction(i);
    }
    vote = error / (1 - error);
    classifier[k].set_vote(log(1 / vote));

    // update weights
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) * pow(vote, 1 - prediction(i));
      weight_sum += weights(i);
    }
    for (int i = 0; i < weights.size(); i++) {
      weights(i) = weights(i) / weight_sum;
    }

  }

  // CREATE CLASSIFIER OUTPUT
  // --------------------------------------------------------------------------------
  for (int i = 0; i < iterations; i++) {
    output[i] = classifier[i].vector();
  }

  return output;
}

