# --------------------------------------------------------------------------------
# TESTS AND PREPARES FEATURES
process_features <- function(features) {

  if (!is.data.frame(features) & !is.matrix(features)) {
    message("ERROR: Features must be data frame or matrix.")
    return(NULL)
  }

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]])) {
      features[[i]] <- factor(features[[i]])
    }

    if (is.factor(features[[i]])) {
      features[[i]] <- as.numeric(features[[i]])
    } else if (!is.numeric(features[[i]])) {
      message(paste("ERROR: Unknown data type in column ", i))
      return(NULL)
    }
  }

  return(data.matrix(features))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES OUTCOMES
process_outcomes <- function(outcomes, features) {

  if (!is.data.frame(outcomes) && !is.vector(outcomes)) {
    message("ERROR: Features must be data frame or vector.")
    return(NULL)
  } else if(is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }

  if (length(outcomes) != nrow(features)) {
    message("ERROR: All training examples must have an outcome.")
  }

  otcm_possibilities <- sort(unique(outcomes))

  if (length(otcm_possibilities) == 2) {
    for (i in seq_along(outcomes)) {
      if (outcomes[[i]] == otcm_possibilities[1]) {
        outcomes[[i]] <- -1
      } else {
        outcomes[[i]] <- 1
      }
    }
  } else {
    message("Error: There are not exactly two distinct outcomes.")
    return(NULL)
  }

  return(as.numeric(outcomes))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES CLASSIFIER
process_classifier <- function(classifier) {

  if (!is.list(classifier)) {
    message("ERROR: Classifier must be a list.")
    return(NULL)
  }

  new_classifier = list()

  for (i in seq_along(classifier)) {
    # feature, direction, vote, split
    if (length(classifier[[i]]$feature) != 1 ||
        length(classifier[[i]]$direction) != 1 ||
        length(classifier[[i]]$vote) != 1 ||
        length(classifier[[i]]$split) < 1) {
      message("ERROR: Classifier not valid.")
      return(NULL)
    }
    new_classifier[[i]] <- c(classifier[[i]]$feature,
                             classifier[[i]]$direction,
                             classifier[[i]]$vote,
                             classifier[[i]]$split)
  }

  return(new_classifier)
}



# --------------------------------------------------------------------------------
# FIND CATEGORICAL VECTOR
find_categorical <- function(features) {
  categorical <- rep(0, ncol(features))

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]]) || is.factor(features[[i]])) {
      categorical[[i]] <- length(unique(features[[i]]))
    }
  }

  return(categorical)
}




