#' sboost Assessment Function
#'
#' Assesses how well an sboost classifier classifies the data.
#'
#' @param features feature set data.frame.
#' @param outcomes outcomes corresponding to the features.
#' @param classifier \emph{sboost_classifier} S3 object output from sboost.
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
#' assess(malware[-1], malware[1], malware_classifier)
#'
#' # mushrooms
#' mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 10, positive = "p")
#' assess(mushrooms[-1], mushrooms[1], mushroom_classifier)
#' @export
assess <- function(features, outcomes, classifier) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  if (is.data.frame(outcomes)) outcomes <- outcomes[[1]]
  processed_features <- process_feature_input(features)
  processed_outcomes <- process_outcome_input(outcomes, features, classifier$outcomes)
  processed_classifier <- process_classifier_input(classifier, features)

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  classifier_assessment <- make_assessment(processed_features, processed_outcomes, processed_classifier)
  classifier_assessment <- process_assessment_output(classifier_assessment, classifier, match.call())


  return(classifier_assessment)
}


# classifier, features, and outcomes must already be processed
make_assessment <- function(features, outcomes, classifier) {

  classifier_assessment <- assess(features, outcomes, classifier)
  colnames(classifier_assessment) <- c("true_positive", "false_negative", "true_negative", "false_positive")
  classifier_assessment <- data.frame(classifier_assessment)

  return(classifier_assessment)
}



