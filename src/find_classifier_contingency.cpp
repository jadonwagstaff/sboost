#include "assessment.h"
#include "stump.h"
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Param: feature matrix, corresponding outcomes, classifier
// Return: contingency table information for each level of the classifier
// [[Rcpp::export]]
NumericMatrix find_classifier_contingency(NumericMatrix& features, NumericVector& outcomes, List& classifier) {
  NumericMatrix contingencies(classifier.size(), 4);
  Assessment classifier_assessment(outcomes.size());
  Stump classifier_stump;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    classifier_assessment.update_predictions(classifier_stump, features, outcomes);
    classifier_assessment.update_contingency(features, outcomes);
    contingencies(i, 0) = classifier_assessment.get_true_positive();
    contingencies(i, 1) = classifier_assessment.get_false_negative();
    contingencies(i, 2) = classifier_assessment.get_true_negative();
    contingencies(i, 3) = classifier_assessment.get_false_positive();
  }

  return contingencies;
}

