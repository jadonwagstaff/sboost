#' sboost Validation Function
#'
#' A k-fold cross validation algorithm for sboost.
#'
#' See sboost documentation for more details.
#' @param feature feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param iterations number of boosts
#' @param k_fold number of cross-validation subsets
#' @param positive is the positive outcome to test for; if NULL, the first in alphebetacal order will be chosen
#' @return Statistics for the test set and the training set.
#' @keywords validation, validate
#' @examples
#' # malware
#' validate(malware[-1], malware[1], iterations = 10, k_fold = 4, positive = 1)
#'
#' # mushrooms
#' validate(mushrooms[-1], mushrooms[1], iterations = 10, k_fold = 4, positive = "p")
#' @export
validate <- function(features, outcomes, iterations = 1, k_fold = 6, positive = NULL) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features and outcomes
  if (is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }
  processed_features <- process_feature_input(features)
  categorical <- find_categorical(features)
  otcm_def <- check_positive_value(outcomes, positive)
  processed_outcomes <- process_outcome_input(outcomes, features, otcm_def)
  if (is.null(processed_outcomes) || is.null(processed_features) || is.null(otcm_def)) {
    return(NULL)
  }

  # create variables
  classifier_list <- list();
  training_assessments <- list();
  testing_assessments <- list();
  rows = nrow(features);


  # DEVELOP CLASSIFIER
  # --------------------------------------------------------------------------------
  for (i in 1:k_fold) {
    classifier_list[[i]] <- make_classifier(processed_features[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows), ],
                                            processed_outcomes[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows)],
                                            categorical,
                                            iterations)
  }


  # TEST CLASSIFIER
  # --------------------------------------------------------------------------------
  for (i in 1:k_fold) {
    training_assessments[[i]] <- make_assessment(processed_features[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows), ],
                                                 processed_outcomes[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows)],
                                                 classifier_list[[i]])
  }
  for (i in 1:k_fold) {
    testing_assessments[[i]] <- make_assessment(processed_features[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows), ],
                                                processed_outcomes[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows)],
                                                classifier_list[[i]])
  }

  # CREATE_OUTPUT
  # --------------------------------------------------------------------------------
  for (i in seq_along(classifier_list)) {
    classifier_list[[i]] <- process_classifier_output(classifier_list[[i]],
                                                      features[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows), ],
                                                      outcomes[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows)],
                                                      otcm_def, match.call())
    training_assessments[[i]] <- process_assessment_output(training_assessments[[i]], classifier_list[[i]], match.call())
    testing_assessments[[i]] <- process_assessment_output(testing_assessments[[i]], classifier_list[[i]], match.call())
  }
  validation <- process_validation_output(training_assessments, testing_assessments, classifier_list, k_fold, match.call())

  return(validation)

}





