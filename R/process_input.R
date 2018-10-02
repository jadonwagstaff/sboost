# --------------------------------------------------------------------------------
# TESTS AND PREPARES FEATURES
process_feature_input <- function(features) {

  if (!is.data.frame(features)) stop("Features must be data frame.")

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]])) {
      features[[i]] <- factor(features[[i]])
    }

    if (is.factor(features[[i]])) {
      features[[i]] <- as.numeric(features[[i]])
    } else if (!is.numeric(features[[i]])) {
      stop(paste("Unknown data type in column ", i))
    }
  }

  return(data.matrix(features))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES OUTCOMES
process_outcome_input <- function(outcomes, features, otcm_def) {

  if (!is.vector(outcomes)) stop("Outcomes must be data frame or vector.")
  if (length(outcomes) != nrow(features)) stop("All training examples must have an outcome.")
  if (length(unique(outcomes)) > 2) stop("Only two distinct outcomes may be assessed.")
  for (i in seq_along(outcomes)) {
    if (outcomes[[i]] == otcm_def["positive"]) {
      outcomes[[i]] <- 1
    } else {
      outcomes[[i]] <- -1
    }
  }

  return(as.numeric(outcomes))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES CLASSIFIER INPUT
process_classifier_input <- function(classifier, features) {

  if (!("sboost_classifier" %in% class(classifier))) stop("Classifier must be an output from sboost.")

  new_classifier = list()

  for (i in 1:nrow(classifier$classifier)) {
    feature <- classifier$classifier$feature[i]
    vote <- classifier$classifier$vote[i]
    orientation <- strsplit(classifier$classifier$orientation[i], "\\|")[[1]]
    if (is.na(classifier$classifier$split[i])) {
      categorical <- 1
      split <- strsplit(classifier$classifier$left_categories[i], "; ")[[1]]
    } else {
      categorical <- 0
      split <- classifier$classifier$split[i]
    }

    # Change feature
    feature <- match(feature, colnames(features))[[1]] - 1

    # Change direction
    if (categorical == 1) {
      if (orientation[[1]] == classifier$outcomes["positive"]) {
        orientation <- 1
      } else {
        orientation <- -1
      }
    } else {
      if (orientation[[1]] == classifier$outcomes["negative"]) {
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
# TESTS POSITIVE SPECIFICATION
# Returns defined outcome possibilities
check_positive_value <- function(outcomes, positive) {
  otcm_p <- sort(unique(outcomes))
  if (length(otcm_p) < 2) stop("There must be two distinct outcomes to use sboost.")
  if (!is.null(positive)) {
    if (!(positive %in% otcm_p)) {
      warning("'positive' variable does not match one of the outcomes. The positive value will be the first outcome in alphabetical order.")
      positive <- NULL
    } else if (positive == otcm_p[[1]]) {
      return(c(positive = otcm_p[[1]], negative = otcm_p[[2]]))
    } else {
      return(c(positive = otcm_p[[2]], negative = otcm_p[[1]]))
    }
  } else {
    return(c(positive = otcm_p[[1]], negative = otcm_p[[2]]))
  }
}



# --------------------------------------------------------------------------------
# FIND CATEGORICAL VECTOR
find_categorical <- function(features) {
  categorical <- rep(0, ncol(features))

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]]) || is.factor(features[[i]])) {
      categorical[[i]] <- length(unique(features[[i]][!is.na(features[[i]])]))
    }
  }

  return(categorical)
}


