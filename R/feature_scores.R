
#' @export
feature_scores <- function(object, features, outcomes) {

  # PREPARE INPUT
  # --------------------------------------------------------------------------------
  if (is.data.frame(outcomes)) outcomes <- outcomes[[1]]
  processed_features <- process_feature_input(features)
  processed_outcomes <- process_outcome_input(outcomes, features, object$outcomes)
  processed_classifier <- process_classifier_input(object, features)

  # ASSESS CLASSIFIER
  # --------------------------------------------------------------------------------
  cpp_scores <- feature_scores_cpp(processed_features, processed_outcomes, processed_classifier)
  scores <- data.frame(matrix(rep(as.numeric(NA), nrow(features) * length(unique(object$classifier$feature))), nrow = nrow(features)))
  feature_names <- c()
  for (i in 1:ncol(cpp_scores)) {
    if (object$classifier$feature[[i]] %in% feature_names) {
      scores[which(feature_names == object$classifier$feature[[i]])] <- scores[which(feature_names == object$classifier$feature[[i]])] + cpp_scores[,i]
    } else {
      feature_names <- c(feature_names, object$classifier$feature[[i]])
      scores[length(feature_names)] <- cpp_scores[,i]
    }
  }
  names(scores) <- feature_names

  return(scores)
}



