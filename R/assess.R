#' sboost Assessment Function
#'
#' Assesses how well an sboost classifier classifies the data.
#'
#' @param object \emph{sboost_classifier} S3 object output from sboost.
#' @param features feature set data.frame.
#' @param outcomes outcomes corresponding to the features.
#' @return An \emph{sboost_assessment} S3 object containing:
#' \describe{
#'   \item{\emph{statistics}}{\emph{stump} - the index of the last decision stump added to the assessment.}
#'     \item{}{\emph{true_positive} - number of true positive predictions.}
#'     \item{}{\emph{false_negative} - number of false negative predictions.}
#'     \item{}{\emph{true_negative} - number of true negative predictions.}
#'     \item{}{\emph{false_positive} - number of false positive predictions.}
#'     \item{}{\emph{prevalence} - true positive / total.}
#'     \item{}{\emph{accuracy} - correct predictions / total.}
#'     \item{}{\emph{sensitivity} - correct predicted positive / true positive.}
#'     \item{}{\emph{specificity} - correct predicted negative / true negative.}
#'     \item{}{\emph{ppv} - correct predicted positive / predicted positive.}
#'     \item{}{\emph{npv} - correct predicted negative / predicted negative.}
#'     \item{}{\emph{f1} - harmonic mean of sensitivity and ppv.}
#'   \item{\emph{classifier}}{sboost \emph{sboost_classifier} object used for assessment.}
#'   \item{\emph{outcomes}}{Shows which outcome was considered as positive and which negative.}
#'   \item{\emph{call}}{Shows the parameters that were used for assessment.}
#' }
#' @seealso \code{\link{sboost}} documentation.
#' @examples
#' # malware
#' malware_classifier <- sboost(malware[-1], malware[1], iterations = 10, positive = 1)
#' assess(malware_classifier, malware[-1], malware[1])
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10, positive = "p")
#' assess(mushrooms[-1], mushrooms[1], mushroom_classifier)
#' @export
assess <- function(object, ...) {
  UseMethod("assess")
}

#' @export
assess.sboost_classifier <- function(object, features, outcomes) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  if (is.data.frame(outcomes)) outcomes <- outcomes[[1]]
  processed_features <- process_feature_input(features)
  processed_outcomes <- process_outcome_input(outcomes, features, object$outcomes)
  processed_classifier <- process_classifier_input(object, features)

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- list(
    contingency = get_contingency(processed_classifier, processed_features, processed_outcomes),
    feature_scores = score_classifier_features(object, processed_classifier, processed_features))
  #classifier_assessment <- process_assessment_output(classifier_assessment, object, match.call())


  return(classifier_assessment)
}


# calls cpp-code for contingency table
# classifier, features, and outcomes must already be processed
get_contingency <- function(classifier, features, outcomes) {

  # CALL C++ CODE TO GET CONTINGENCY TABLE
  # --------------------------------------------------------------------------------
  contingency <- get_contingency_cpp(features, outcomes, classifier)
  colnames(contingency) <- c("true_positive", "false_negative", "true_negative", "false_positive")
  contingency <- data.frame(contingency)

  return(contingency)
}


# calls cpp-code for classifier feature scores
# object must be sboost_classifier, classifier and features must already be processed
score_classifier_features <- function(object, classifier, features) {

  # CALL C++ CODE TO SCORE STUMPS
  # --------------------------------------------------------------------------------
  cpp_scores <- score_classifier_features_cpp(classifier, features)

  # ADD TOGETHER SCORES OF STUMPS ON THE SAME FEATURES
  # --------------------------------------------------------------------------------
  scores <- data.frame(matrix(rep(as.numeric(NA), nrow(features) * length(unique(object$classifier$feature))), nrow = nrow(features)))
  feature_names <- c()
  for (i in 1:ncol(cpp_scores)) {
    if (object$classifier$feature[[i]] %in% feature_names) {
      # If a stump with this feature has already been added, add this stump score to the score for that feature
      scores[which(feature_names == object$classifier$feature[[i]])] <- scores[which(feature_names == object$classifier$feature[[i]])] + cpp_scores[,i]
    } else {
      # If a stump with this feature has not already been added, add stump scores to next empty column
      feature_names <- c(feature_names, object$classifier$feature[[i]])
      scores[length(feature_names)] <- cpp_scores[,i]
    }
  }
  names(scores) <- feature_names

  return(scores)
}

