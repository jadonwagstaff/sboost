# --------------------------------------------------------------------------------
# TESTS AND PREPARES FEATURES
process_feature_input <- function(features) {

  if (!is.data.frame(features)) {
    message("ERROR: Features must be data frame.")
    return(NULL)
  }

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]])) {
      features[[i]] <- factor(features[[i]])
    }

    if (is.factor(features[[i]])) {
      features[[i]] <- as.numeric(addNA(features[[i]]))
    } else if (!is.numeric(features[[i]])) {
      message(paste("ERROR: Unknown data type in column ", i))
      return(NULL)
    }
  }

  return(data.matrix(features))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES OUTCOMES
process_outcome_input <- function(outcomes, features) {

  if (!is.data.frame(outcomes) && !is.vector(outcomes)) {
    message("ERROR: Features must be data frame or vector.")
    return(NULL)
  } else if(is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }

  if (length(outcomes) != nrow(features)) {
    message("ERROR: All training examples must have an outcome.")
    return(NULL)
  }

  otcm_possibilities <- sort(unique(outcomes))

  if (length(otcm_possibilities) <= 2) {
    for (i in seq_along(outcomes)) {
      if (outcomes[[i]] == otcm_possibilities[1]) {
        outcomes[[i]] <- -1
      } else {
        outcomes[[i]] <- 1
      }
    }
  } else {
    message("Error: Only two distinct outcomes may be assessed.")
    return(NULL)
  }

  return(as.numeric(outcomes))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES CLASSIFIER INPUT
process_classifier_input <- function(classifier, features) {

  if (!is.data.frame(classifier)) {
    message("ERROR: Classifier must be a data frame.")
    return(NULL)
  }

  if (ncol(classifier) != 6) {
    message("ERROR: Classifier is the wrong format.")
    return(NULL)
  }

  new_classifier = list()

  # determine outcomes
  otcm_possibilities <- sort(strsplit(classifier$orientation[1], "\\|")[[1]])

  for (i in 1:nrow(classifier)) {
    feature <- classifier$feature[i]
    vote <- classifier$vote[i]
    orientation <- strsplit(classifier$orientation[i], "\\|")[[1]]
    if (is.na(classifier$split[i])) {
      categorical <- 1
      split <- strsplit(classifier$left_categories[i], "; ")[[1]]
    } else {
      categorical <- 0
      split <- classifier$split[i]
    }

    # Change feature
    feature <- match(feature, colnames(features))[[1]] - 1

    # Change direction
    if (categorical == 1) {
      if (orientation[[1]] == otcm_possibilities[[1]]) {
        orientation <- 1
      } else {
        orientation <- -1
      }
    } else {
      if (orientation[[1]] == otcm_possibilities[[2]]) {
        orientation <- 1
      } else {
        orientation <- -1
      }
    }

    # Change split
    if (categorical == 1) {
      feature_levels <- levels(addNA(factor(features[[feature + 1]])))
      for (j in seq_along(split)) {
        if (!is.na(match(split[[j]], feature_levels, nomatch = NA, incomparables = NA))) {
          split[[j]] <- match(split[[j]], feature_levels)
        }
      }
    }

    new_classifier[[i]] <- as.numeric(c(feature, orientation, vote, categorical, split))

  }

  return(new_classifier)
}



# --------------------------------------------------------------------------------
# TESTS AND POSITIVE SPECIFICATION
# Returns true if 'positive' is first outcome, false if not
check_positive_value <- function(otcm_possibilities, positive) {
  if (is.null(positive)) {
    return(TRUE)
  }
  otcm_possibilities <- sort(otcm_possibilities)
  if (!positive %in% otcm_possibilities) {
    message("ERROR: 'positive' variable must match one of the outcomes.")
    return(NULL)
  }
  if (positive == otcm_possibilities[[1]]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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


