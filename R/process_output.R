#' @importFrom rlang .data
#' @importFrom stats sd

# --------------------------------------------------------------------------------
# PREPARES CLASSIFIER OUTPUT
process_classifier_output <- function(classifier, features, outcomes, otcm_def, call) {

  # create classifier data frame
  clfr <- data.frame(matrix(ncol = 6, nrow = length(classifier)))
  colnames(clfr) <- c("stump", "feature", "vote", "orientation", "split", "left_categories")

  # set output values
  for (i in seq_along(classifier)) {
    feature <- classifier[[i]][[1]] + 1
    orientation <- classifier[[i]][[2]]
    vote <- classifier[[i]][[3]]
    categorical <- classifier[[i]][[4]]
    split <- classifier[[i]][c(-1, -2, -3, -4)]

    # stump
    clfr$stump[i] <- i

    # feature name
    clfr$feature[i] <- colnames(features)[[feature]]

    # vote
    clfr$vote[i] <- vote

    if (categorical == 0) {
      # orientation
      if (orientation == 1) {
        clfr$orientation[i] <- paste0(otcm_def$negative, "|", otcm_def$positive)
      } else {
        clfr$orientation[i] <- paste0(otcm_def$positive, "|", otcm_def$negative)
      }
      # split
      clfr$split[i] <- split
      clfr$left_categories[i] <- NA
    }
    if (categorical == 1) {
      # orientation
      clfr$orientation[i] <- paste0(otcm_def$positive, "|", otcm_def$negative)
      # categories
      temp_split <- rep(NA, length(split))
      feature_levels <- levels(addNA(factor(features[[feature]])))
      for (j in 1:length(split)) {
        temp_split[[j]] <- feature_levels[[split[[j]]]]
      }
      clfr$left_categories[i] <- paste(temp_split, collapse = "; ")
      clfr$split[i] <- NA
    }
  }

  # Training set information
  training = data.frame(stumps = nrow(clfr),
                        features = ncol(features),
                        instances = nrow(features),
                        positive_prevalence = sum(outcomes == otcm_def$positive) / length(outcomes))

  # Assessment
  output <- list(classifier = clfr, outcomes = otcm_def, training = training, call = call)
  class(output) <- "sboost_classifier"

  return(output)
}

#' @export
print.sboost_classifier <- function(x, ...) {
  cat("SBOOST CLASSIFIER SUMMARY\n")
  cat(" ----------------------- \n")
  cat("Number of stumps trained: ", x$training$stumps, "\n", sep = "")
  cat("Number of training features: ", x$training$features, "\n\n", sep = "")

  cat("Number of training instances: ", x$training$instances, "\n", sep = "")
  cat("Positive outcome: ", as.character(x$outcomes$positive), "\n", sep = "")
  cat("Positive prevalence: ", x$training$positive_prevalence, "\n", sep = "")
  cat("Negative outcome: ", as.character(x$outcomes$negative), "\n", sep = "")
}






# --------------------------------------------------------------------------------
# PREPARE ASSESSMENT OUTPUT
process_assessment_output <- function(assessment, classifier, call) {
  # Statistics
  statistics <- dplyr::mutate(assessment,
                              stump = 1:nrow(assessment))
  statistics <- dplyr::select(statistics, .data$stump, .data$true_positive, .data$false_negative, .data$true_negative, .data$false_positive)
  statistics <- dplyr::mutate(statistics,
                              prevalence = (.data$true_positive + .data$false_negative) / (.data$true_positive + .data$true_negative + .data$false_positive + .data$false_negative),
                              accuracy = (.data$true_positive + .data$true_negative) / (.data$true_positive + .data$true_negative + .data$false_positive + .data$false_negative),
                              sensitivity = .data$true_positive / (.data$true_positive + .data$false_negative),
                              specificity = .data$true_negative / (.data$true_negative + .data$false_positive),
                              ppv = .data$true_positive / (.data$true_positive + .data$false_positive),
                              npv = .data$true_negative / (.data$true_negative + .data$false_negative),
                              f1 = (2 * .data$ppv * .data$sensitivity) / (.data$ppv + .data$sensitivity))

  # Output
  output <- list(statistics = statistics, classifier = classifier, outcomes = classifier$outcomes, call = call)
  class(output) <- "sboost_assessment"

  return(output)
}

#' @export
print.sboost_assessment <- function(x, ...) {
  last <- nrow(x$statistics)

  cat("SBOOST ASSESSMENT SUMMARY\n", sep = "")
  cat(" ----------------------- \n", sep = "")
  cat("Number of classifier stumps used: ", x$statistics$stump[last], "\n", sep = "")
  cat("Number of instances assessed: ", x$statistics$true_positive[last] + x$statistics$false_negative[last] + x$statistics$true_negative[last] + x$statistics$false_positive[last], "\n\n", sep = "")

  cat("Positive outcome: ", as.character(x$outcomes$positive), "\n", sep = "")
  cat("Prevalence: ", x$statistics$prevalence[last], "\n", sep = "")
  cat("Negative outcome: ", as.character(x$outcomes$negative), "\n\n", sep = "")

  cat("Accuracy: ", x$statistics$accuracy[last], "\n", sep = "")
  cat("Sensitivity: ", x$statistics$sensitivity[last], "\n", sep = "")
  cat("Specificity: ", x$statistics$specificity[last], "\n", sep = "")
  cat("PPV: ", x$statistics$ppv[last], "\n", sep = "")
  cat("NPV: ", x$statistics$npv[last], "\n", sep = "")
  cat("F1: ", x$statistics$f1[last], "\n", sep = "")
}







# --------------------------------------------------------------------------------
# PREPARE VALIDATION OUTPUT
process_validation_output <- function(training_assessments, testing_assessments, classifier_list, k_fold, call) {

  training_statistics <- training_assessments[[1]]$statistics
  testing_statistics <- testing_assessments[[1]]$statistics
  for (i in seq_along(training_assessments)) {
    training_statistics <- rbind(training_statistics, training_assessments[[i]]$statistics)
    testing_statistics <- rbind(testing_statistics, testing_assessments[[i]]$statistics)
  }

  training_statistics <- dplyr::group_by(training_statistics, .data$stump)
  training_statistics <- dplyr::summarise(training_statistics,
                                          accuracy_mean = mean(.data$accuracy),
                                          accuracy_sd = sd(.data$accuracy),
                                          sensitivity_mean = mean(.data$sensitivity),
                                          sensitivity_sd = sd(.data$sensitivity),
                                          specificity_mean = mean(.data$specificity),
                                          specificity_sd = sd(.data$specificity),
                                          ppv_mean = mean(.data$ppv),
                                          ppv_sd = sd(.data$ppv),
                                          npv_mean = mean(.data$npv),
                                          npv_sd = sd(.data$npv),
                                          f1_mean = mean(.data$f1),
                                          f1_sd = sd(.data$f1))
  testing_statistics <- dplyr::group_by(testing_statistics, .data$stump)
  testing_statistics <- dplyr::summarise(testing_statistics,
                                         accuracy_mean = mean(.data$accuracy),
                                         accuracy_sd = sd(.data$accuracy),
                                         sensitivity_mean = mean(.data$sensitivity),
                                         sensitivity_sd = sd(.data$sensitivity),
                                         specificity_mean = mean(.data$specificity),
                                         specificity_sd = sd(.data$specificity),
                                         ppv_mean = mean(.data$ppv),
                                         ppv_sd = sd(.data$ppv),
                                         npv_mean = mean(.data$npv),
                                         npv_sd = sd(.data$npv),
                                         f1_mean = mean(.data$f1),
                                         f1_sd = sd(.data$f1))

  output <- list(training_statistics = training_statistics,
                 testing_statistics = testing_statistics,
                 training_assessments = training_assessments,
                 testing_assessments = testing_assessments,
                 classifier_list = classifier_list,
                 outcomes = classifier_list[[1]]$outcomes,
                 k_fold = k_fold,
                 call = call)
  class(output) <- "sboost_validation"
  return(output)

}

#' @export
print.sboost_validation <- function(x, ...) {
  last <- nrow(x$training_statistics)

  cat("SBOOST VALIDATION SUMMARY\n", sep = "")
  cat(" ----------------------- \n", sep = "")
  cat("Number of validation steps: ", x$k_fold, "\n", sep = "")
  cat("Number of training features: ", x$classifier_list[[1]]$training$features, "\n", sep = "")
  cat("Number of stumps per training set: ", x$classifier_list[[1]]$training$stumps, "\n", sep = "")

  cat("Approximate instances per training set: ", x$classifier_list[[1]]$training$instances, "\n", sep = "")
  cat("Approximate instances per testing set: ", floor(x$classifier_list[[1]]$training$instances / (x$k_fold - 1)), "\n", sep = "")
  cat("Positive outcome: ", as.character(x$outcomes$positive), "\n", sep = "")
  cat("Negative outcome: ", as.character(x$outcomes$negative), "\n\n", sep = "")

  cat("Mean accuracy (SD)\n")
  cat(" Training: ", x$training_statistics$accuracy_mean[last], " (", x$training_statistics$accuracy_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$accuracy_mean[last], " (", x$testing_statistics$accuracy_sd[last], ")\n", sep = "")
  cat("Mean sensitivity (SD)\n")
  cat(" Training: ", x$training_statistics$sensitivity_mean[last], " (", x$training_statistics$sensitivity_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$sensitivity_mean[last], " (", x$testing_statistics$sensitivity_sd[last], ")\n", sep = "")
  cat("Mean specificity (SD)\n")
  cat(" Training: ", x$training_statistics$specificity_mean[last], " (", x$training_statistics$specificity_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$specificity_mean[last], " (", x$testing_statistics$specificity_sd[last], ")\n", sep = "")
  cat("Mean PPV (SD)\n")
  cat(" Training: ", x$training_statistics$ppv_mean[last], " (", x$training_statistics$ppv_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$ppv_mean[last], " (", x$testing_statistics$ppv_sd[last], ")\n", sep = "")
  cat("Mean NPV (SD)\n")
  cat(" Training: ", x$training_statistics$npv_mean[last], " (", x$training_statistics$npv_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$npv_mean[last], " (", x$testing_statistics$npv_sd[last], ")\n", sep = "")
  cat("Mean F1 (SD)\n")
  cat(" Training: ", x$training_statistics$f1_mean[last], " (", x$training_statistics$f1_sd[last], ")\n", sep = "")
  cat(" Testing: ", x$testing_statistics$f1_mean[last], " (", x$testing_statistics$f1_sd[last], ")\n", sep = "")
}






