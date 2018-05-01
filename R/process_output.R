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
print.sboost_classifier <- function(c) {
  cat("SBOOST CLASSIFIER SUMMARY\n")
  cat(" ----------------------- \n")
  cat("Number of stumps trained: ", c$training$stumps, "\n", sep = "")
  cat("Number of training features: ", c$training$features, "\n\n", sep = "")

  cat("Number of training instances: ", c$training$instances, "\n", sep = "")
  cat("Positive outcome: ", as.character(c$outcomes$positive), "\n", sep = "")
  cat("Positive prevalence: ", c$training$positive_prevalence, "\n", sep = "")
  cat("Negative outcome: ", as.character(c$outcomes$negative), "\n", sep = "")
}






# --------------------------------------------------------------------------------
# PREPARE ASSESSMENT OUTPUT
process_assessment_output <- function(assessment, classifier, call) {
  # Statistics
  statistics <- dplyr::mutate(assessment,
                              stump = 1:nrow(assessment))
  statistics <- dplyr::select(statistics, stump, true_positive, false_negative, true_negative, false_positive)
  statistics <- dplyr::mutate(statistics,
                              prevalence = (true_positive + false_negative) / (true_positive + true_negative + false_positive + false_negative),
                              accuracy = (true_positive + true_negative) / (true_positive + true_negative + false_positive + false_negative),
                              sensitivity = true_positive / (true_positive + false_negative),
                              specificity = true_negative / (true_negative + false_positive),
                              ppv = true_positive / (true_positive + false_positive),
                              npv = true_negative / (true_negative + false_negative),
                              f1 = (2 * ppv * sensitivity) / (ppv + sensitivity))

  # Output
  output <- list(statistics = statistics, classifier = classifier, outcomes = classifier$outcomes, call = call)
  class(output) <- "sboost_assessment"

  return(output)
}

#' @export
print.sboost_assessment <- function(a) {
  last <- nrow(a$statistics)

  cat("SBOOST ASSESSMENT SUMMARY\n", sep = "")
  cat(" ----------------------- \n", sep = "")
  cat("Number of classifier stumps used: ", a$statistics$stump[last], "\n", sep = "")
  cat("Number of instances assessed: ", a$statistics$true_positive[last] + a$statistics$false_negative[last] + a$statistics$true_negative[last] + a$statistics$false_positive[last], "\n\n", sep = "")

  cat("Positive outcome: ", as.character(a$outcomes$positive), "\n", sep = "")
  cat("Prevalence: ", a$statistics$prevalence[last], "\n", sep = "")
  cat("Negative outcome: ", as.character(a$outcomes$negative), "\n\n", sep = "")

  cat("Accuracy: ", a$statistics$accuracy[last], "\n", sep = "")
  cat("Sensitivity: ", a$statistics$sensitivity[last], "\n", sep = "")
  cat("Specificity: ", a$statistics$specificity[last], "\n", sep = "")
  cat("PPV: ", a$statistics$ppv[last], "\n", sep = "")
  cat("NPV: ", a$statistics$npv[last], "\n", sep = "")
  cat("F1: ", a$statistics$f1[last], "\n", sep = "")
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

  training_statistics <- dplyr::group_by(training_statistics, stump)
  training_statistics <- dplyr::summarise(training_statistics,
                                          accuracy_mean = mean(accuracy),
                                          accuracy_sd = sd(accuracy),
                                          sensitivity_mean = mean(sensitivity),
                                          sensitivity_sd = sd(sensitivity),
                                          specificity_mean = mean(specificity),
                                          specificity_sd = sd(specificity),
                                          ppv_mean = mean(ppv),
                                          ppv_sd = sd(ppv),
                                          npv_mean = mean(npv),
                                          npv_sd = sd(npv),
                                          f1_mean = mean(f1),
                                          f1_sd = sd(f1))
  testing_statistics <- dplyr::group_by(testing_statistics, stump)
  testing_statistics <- dplyr::summarise(testing_statistics,
                                         accuracy_mean = mean(accuracy),
                                         accuracy_sd = sd(accuracy),
                                         sensitivity_mean = mean(sensitivity),
                                         sensitivity_sd = sd(sensitivity),
                                         specificity_mean = mean(specificity),
                                         specificity_sd = sd(specificity),
                                         ppv_mean = mean(ppv),
                                         ppv_sd = sd(ppv),
                                         npv_mean = mean(npv),
                                         npv_sd = sd(npv),
                                         f1_mean = mean(f1),
                                         f1_sd = sd(f1))

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
print.sboost_validation <- function(v) {
  last <- nrow(v$training_statistics)

  cat("SBOOST VALIDATION SUMMARY\n", sep = "")
  cat(" ----------------------- \n", sep = "")
  cat("Number of validation steps: ", v$k_fold, "\n", sep = "")
  cat("Number of training features: ", v$classifier_list[[1]]$training$features, "\n", sep = "")
  cat("Number of stumps per training set: ", v$classifier_list[[1]]$training$stumps, "\n", sep = "")

  cat("Approximate instances per training set: ", v$classifier_list[[1]]$training$instances, "\n", sep = "")
  cat("Approximate instances per testing set: ", floor(v$classifier_list[[1]]$training$instances / (v$k_fold - 1)), "\n", sep = "")
  cat("Positive outcome: ", as.character(v$outcomes$positive), "\n", sep = "")
  cat("Negative outcome: ", as.character(v$outcomes$negative), "\n\n", sep = "")

  cat("Mean accuracy (SD)\n")
  cat(" Training: ", v$training_statistics$accuracy_mean[last], " (", v$training_statistics$accuracy_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$accuracy_mean[last], " (", v$testing_statistics$accuracy_sd[last], ")\n", sep = "")
  cat("Mean sensitivity (SD)\n")
  cat(" Training: ", v$training_statistics$sensitivity_mean[last], " (", v$training_statistics$sensitivity_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$sensitivity_mean[last], " (", v$testing_statistics$sensitivity_sd[last], ")\n", sep = "")
  cat("Mean specificity (SD)\n")
  cat(" Training: ", v$training_statistics$specificity_mean[last], " (", v$training_statistics$specificity_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$specificity_mean[last], " (", v$testing_statistics$specificity_sd[last], ")\n", sep = "")
  cat("Mean PPV (SD)\n")
  cat(" Training: ", v$training_statistics$ppv_mean[last], " (", v$training_statistics$ppv_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$ppv_mean[last], " (", v$testing_statistics$ppv_sd[last], ")\n", sep = "")
  cat("Mean NPV (SD)\n")
  cat(" Training: ", v$training_statistics$npv_mean[last], " (", v$training_statistics$npv_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$npv_mean[last], " (", v$testing_statistics$npv_sd[last], ")\n", sep = "")
  cat("Mean F1 (SD)\n")
  cat(" Training: ", v$training_statistics$f1_mean[last], " (", v$training_statistics$f1_sd[last], ")\n", sep = "")
  cat(" Testing: ", v$testing_statistics$f1_mean[last], " (", v$testing_statistics$f1_sd[last], ")\n", sep = "")
}






