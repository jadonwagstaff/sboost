#include "assessment.h"
#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Param: feature matrix, corresponding outcomes, classifier
// Return: contingency table information for each level of the classifier
// [[Rcpp::export]]
NumericVector make_predictions(const NumericMatrix& features, const List& classifier) {
  Assessment classifier_assessment(features.nrow());
  Stump classifier_stump;
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_assessment.update_predictions(classifier_stump, features);
    }
  }

  return classifier_assessment.get_predictions();
}



// Param: feature matrix, corresponding outcomes, classifier
// Return: contingency table information for each level of the classifier
// [[Rcpp::export]]
NumericMatrix find_classifier_contingency(NumericMatrix& features, NumericVector& outcomes, List& classifier) {
  NumericMatrix contingencies(classifier.size(), 4);
  Assessment classifier_assessment(outcomes.size());
  Stump classifier_stump;
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_assessment.update_predictions(classifier_stump, features);
      classifier_assessment.update_contingency(features, outcomes);
    }
    contingencies(i, 0) = classifier_assessment.get_true_positive();
    contingencies(i, 1) = classifier_assessment.get_false_negative();
    contingencies(i, 2) = classifier_assessment.get_true_negative();
    contingencies(i, 3) = classifier_assessment.get_false_positive();
  }

  return contingencies;
}



