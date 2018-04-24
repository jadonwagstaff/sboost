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
  processed_features <- process_feature_input(features)
  processed_outcomes <- process_outcome_input(outcomes, features)
  categorical <- find_categorical(features)

  if(is.data.frame(outcomes)) {
    positive_matched <- check_positive_value(unique(outcomes[[1]]), positive)
  } else {
    positive_matched <- check_positive_value(unique(outcomes), positive)
  }
  if (is.null(outcomes) || is.null(features) || is.null(positive_matched)) {
    return(NULL)
  }
  if (length(unique(processed_outcomes)) < 2) {
    message("ERROR: There must be two distinct outcomes to use sboost.")
    return(NULL)
  }

  # create variables
  classifier_list <- list();
  assessment_list <- list();
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

  # training
  for (i in 1:k_fold) {
    assessment_list[[i]] <- assess_classifier_internal(processed_features[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows), ],
                                                       processed_outcomes[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows)],
                                                       classifier_list[[i]],
                                                       positive_matched)
  }

  for (i in 1:k_fold) {
    assessment_list[[i]] <- dplyr::mutate(assessment_list[[i]],
                                          number = 1:iterations)
  }

  training <- dplyr::bind_rows(assessment_list)

  # testing
  for (i in 1:k_fold) {
    assessment_list[[i]] <- assess_classifier_internal(processed_features[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows), ],
                                                       processed_outcomes[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows)],
                                                       classifier_list[[i]],
                                                       positive_matched)
  }

  for (i in 1:k_fold) {
    assessment_list[[i]] <- dplyr::mutate(assessment_list[[i]],
                                          number = 1:iterations)
  }

  testing <- dplyr::bind_rows(assessment_list)

  # combined
  assessment <- list("training" = training, "testing" = testing)
  for (j in 1:2) {
    assessment[[j]] <- dplyr::group_by(assessment[[j]], number)
    assessment[[j]] <- dplyr::summarise(assessment[[j]],
                                   k_fold = k_fold,
                                   true_positive_mean = mean(true_positive),
                                   true_positive_sd = sd(true_positive),
                                   false_negative_mean = mean(false_negative),
                                   false_negative_sd = sd(false_negative),
                                   true_negative_mean = mean(true_negative),
                                   true_negative_sd = sd(true_negative),
                                   false_positive_mean = mean(false_positive),
                                   false_negative_sd = sd(false_positive),
                                   accuracy_mean = mean(accuracy),
                                   accuracy_sd = sd(accuracy),
                                   recall_mean = mean(recall),
                                   recall_sd = sd(recall),
                                   specificity_mean = mean(specificity),
                                   specificity_sd = sd(specificity),
                                   precision_mean = mean(precision),
                                   precision_sd = sd(precision),
                                   f1_mean = mean(f1),
                                   f1_sd = sd(f1))
  }


  return(assessment)

}





