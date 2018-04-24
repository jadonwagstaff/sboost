#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Param: feature matrix, corresponding outcomes, classifier
// Return: contingency table information for each level of the classifier
// [[Rcpp::export]]
NumericVector predict(const NumericMatrix& features, const List& classifier) {
  Stump::populate_data(features);
  NumericVector predictions(features.nrow());
  Stump classifier_stump;
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_stump.update_predictions(predictions);
    }
  }

  return predictions;
}



// Param: feature matrix, corresponding outcomes, classifier
// Return: contingency table information for each level of the classifier
// [[Rcpp::export]]
NumericMatrix assess(const NumericMatrix& features, const NumericVector& outcomes, const List& classifier, int interval) {
  Stump::populate_data(features, outcomes);
  NumericVector predictions(features.nrow());

  NumericMatrix contingencies(classifier.size() / interval, 4);
  Stump classifier_stump;
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_stump.update_predictions(predictions);
      if ((i + 1) % interval == 0) {
        // true_positive, false_negative, true_negative, false_positive
        contingencies(((i + 1) / interval) - 1, _) = classifier_stump.get_contingencies(predictions);
      }
    }
  }

  return contingencies;
}



