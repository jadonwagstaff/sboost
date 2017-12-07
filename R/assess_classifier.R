#' assess_classifier
#'
#' This function finds determines how well a set of stump classifiers fit a given dataset
#' @param feature feature set
#' @param outcomes index corresponding to the features
#' @param classifier must be output from sboost
#' @return Assessment after each additional stump is included in classifier
#' @keywords stump, assess, assessment, f1, accuracy
#' @export
assess_classifier <- function(features, outcomes, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features and outcomes
  classifier <- process_classifier(classifier)
  features <- process_features(features)
  outcomes <- process_outcomes(outcomes, features)
  if (is.null(outcomes) || is.null(features) || is.null(classifier)) {
    return(NULL)
  }


  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
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
