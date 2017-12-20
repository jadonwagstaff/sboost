#' assess_model
#'
#' This function finds determines how well a model fits a given dataset
#' @param feature feature set
#' @param outcomes index corresponding to the features
#' @param model must be output from sboost
#' @return Assessment after each additional line in the model
#' @keywords assess, assessment, f1, accuracy
#' @export
assess_model <- function(features, outcomes, model) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features and outcomes
  processed_features <- process_features(features)
  processed_outcomes <- process_outcomes(outcomes, features)
  if (is.null(outcomes) || is.null(features)) {
    return(NULL)
  }

  # test and prepare classifier or regressor
  if (ncol(model) == 5) {
    classifier <- process_classifier(model, features, outcomes)
    regressor <- NULL
  } else if (ncol(model) == 6) {
    regressor <- process_regressor(model, features, outcomes)
    classifier <- NULL
  } else {
    message("ERROR: Model is not in correct format.")
    return(NULL)
  }

  # ASSESS MODEL
  # --------------------------------------------------------------------------------
  model_assessment <- NULL
  if (!is.null(classifier)) {
    model_assessment <- assess_classifier(processed_features, processed_outcomes, classifier)
  } else if (!is.null(regressor)) {
    model_assessment <- assess_regressor(processed_features, processed_outcomes, regressor)
  }


  return(model_assessment)
}


# classifier, features, and outcomes must already be processed
assess_classifier <- function(features, outcomes, classifier) {
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

# classifier, features, and outcomes must already be processed
assess_regressor <- function(features, outcomes, regressor) {
  regressor_assessment <- find_regressor_contingency(features, outcomes, regressor)
  sse <- rep(0, length(regressor))
  ssto <- sum((outcomes - mean(outcomes))^2)
  for (i in 1:ncol(regressor_assessment)) {
    sse[[i]] <- sum((outcomes - regressor_assessment[,i])^2)
  }
  output <- data.frame(MSE = sse / length(outcomes), R2 = 1 - sse / ssto)

  return(output)
}

