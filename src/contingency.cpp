#include "assessment.h"
#include "predictor.h"
#include "stump.h"
#include "line.h"
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
  NumericVector temp;

  for (int i = 0; i < classifier.size(); i++) {
    NumericVector temp = classifier[i];
    classifier_stump = Stump(temp);
    if (!std::isnan(classifier_stump.get_vote())) {
      classifier_assessment.update_predictions(classifier_stump, features, outcomes);
      classifier_assessment.update_contingency(features, outcomes);
    }
    contingencies(i, 0) = classifier_assessment.get_true_positive();
    contingencies(i, 1) = classifier_assessment.get_false_negative();
    contingencies(i, 2) = classifier_assessment.get_true_negative();
    contingencies(i, 3) = classifier_assessment.get_false_positive();
  }

  return contingencies;
}

// Param: feature matrix, corresponding outcomes, classifier
// Return: matrix with columns containing predictions for each additional line
// [[Rcpp::export]]
NumericMatrix find_regressor_contingency(NumericMatrix& features, NumericVector& outcomes, List& regressor) {
  // create variables
  NumericMatrix predictions(outcomes.size(), regressor.size());
  Line regressor_line;
  NumericVector temp;
  std::vector<Predictor> predictors;
  for (int i = 0; i < outcomes.size(); i++) {
    predictors.push_back(Predictor(features(i, _), outcomes(i)));
  }

  // make predictions
  for (int j = 0; j < regressor.size(); j++) {
    NumericVector temp = regressor[j];
    regressor_line = Line(temp);
    for (int i = 0; i < predictors.size(); i++) {
      predictors[i].update_prediction(regressor_line);
      predictions(i, j) = predictors[i].prediction();
    }
  }

  return predictions;
}

