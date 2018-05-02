#include "stump.h"
#include <Rcpp.h>
using namespace Rcpp;

// Param: feature matrix, classifier
// Return: prediction scores based on classifier
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
NumericMatrix assess(const NumericMatrix& features, const NumericVector& outcomes, const List& classifier) {
  Stump::populate_data(features, outcomes);
  NumericVector predictions(features.nrow());

  NumericMatrix contingencies(classifier.size(), 4);
  Stump classifier_stump;
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_stump.update_predictions(predictions);
      // true_positive, false_negative, true_negative, false_positive
      contingencies(i, _) = classifier_stump.get_contingencies(predictions);
    }
  }

  return contingencies;
}



