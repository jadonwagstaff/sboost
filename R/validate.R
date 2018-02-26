#' sboost Validation Function
#'
#' A k-fold cross validation algorithm for sboost.
#'
#' See sboost documentation for more details.
#' @param feature feature set data.frame
#' @param outcomes outcomes corresponding to the features
#' @param iterations number of boosts
#' @param k_fold number of cross-validation subsets
#' @return Statistics for the test set and the training set.
#' @keywords validation, validate
#' @export
validate <- function(features, outcomes, iterations = 1, k_fold = 6) {
  # PREPARE INPUT
  # --------------------------------------------------------------------------------

  # test and prepare features and outcomes
  categorical <- find_categorical(features)
  features <- process_features(features)
  outcomes <- process_outcomes(outcomes, features)
  if (is.null(outcomes) || is.null(features)) {
    return(NULL)
  }

  # create variables
  classifier_list <- list();
  assessment_list <- list();
  rows = nrow(features);


  # DEVELOP CLASSIFIER
  # --------------------------------------------------------------------------------
  for (i in 1:k_fold) {
    classifier_list[[i]] <- make_classifier(features[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows), ],
                                            outcomes[-(((i - 1) / k_fold) * rows):-((i / k_fold) * rows)],
                                            categorical,
                                            iterations)
  }

  # TEST CLASSIFIER
  # --------------------------------------------------------------------------------

  # training
  for (i in 1:k_fold) {
    assessment_list[[i]] <- assess_classifier_internal(features[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows), ],
                                                       outcomes[-((((i - 1) / k_fold) * rows) + 1):-((i / k_fold) * rows)],
                                                       classifier_list[[i]])
  }

  for (i in 1:k_fold) {
    assessment_list[[i]] <- dplyr::mutate(assessment_list[[i]],
                                          number = 1:iterations)
  }

  training <- dplyr::bind_rows(assessment_list)

  # testing
  for (i in 1:k_fold) {
    assessment_list[[i]] <- assess_classifier_internal(features[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows), ],
                                                       outcomes[((((i - 1) / k_fold) * rows) + 1):((i / k_fold) * rows)],
                                                       classifier_list[[i]])
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





