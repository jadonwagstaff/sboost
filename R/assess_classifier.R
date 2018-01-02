#' assess_model
#'
#' This function finds determines how well a model fits a given dataset
#' @param feature feature set
#' @param outcomes index corresponding to the features
#' @param classifier must be output from sboost
#' @return Assessment after each additional line in the model
#' @keywords assess, assessment, f1, accuracy
#' @export
assess_classifier <- function(features, outcomes, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features and outcomes
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  classifier <- process_classifier(classifier, features, outcomes)
  if (is.null(outcomes) || is.null(features) || is.null(classifier)) {
    return(NULL)
  }

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- assess_classifier_internal(processed_features, processed_outcomes, classifier)


  return(classifier_assessment)
}


# classifier, features, and outcomes must already be processed
assess_classifier_internal <- function(features, outcomes, classifier) {
  classifier_assessment <- find_classifier_contingency(features, outcomes, classifier)
  colnames(classifier_assessment) <- c("true_positive", "false_negative", "true_negative", "false_positive")

  classifier_assessment <- data.frame(classifier_assessment)
  classifier_assessment <- dplyr::mutate(classifier_assessment,
                                         accuracy = (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative),
                                         recall = true_positive / (true_positive + false_negative),
                                         specificity = true_negative / (true_negative + false_positive),
                                         precision = true_positive / (true_positive + false_positive),
                                         f1 = (2 * precision * recall) / (precision + recall))

  return(classifier_assessment)
}



