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
process_outcomes <- function(outcomes, features) {

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

  if (length(otcm_possibilities) == 2) {
    for (i in seq_along(outcomes)) {
      if (outcomes[[i]] == otcm_possibilities[1]) {
        outcomes[[i]] <- -1
      } else {
        outcomes[[i]] <- 1
      }
    }
  } else {
    message("Error: There are not exactly two distinct outcomes\n or outcomes are not numeric.")
    return(NULL)
  }

  return(as.numeric(outcomes))
}



# --------------------------------------------------------------------------------
# TESTS AND PREPARES CLASSIFIER INPUT
process_classifier <- function(classifier, features) {

  if (!is.data.frame(classifier)) {
    message("ERROR: Classifier must be a data frame.")
    return(NULL)
  }

  if (ncol(classifier) != 5) {
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
check_positive <- function(otcm_possibilities, positive) {
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
# PREPARES CLASSIFIER OUTPUT
prepare_classifier <- function(classifier, features, outcomes) {

  # create output data frame
  output <- data.frame(matrix(ncol = 5, nrow = length(classifier)))
  colnames(output) <- c("feature", "vote", "orientation", "split", "left_categories")

  # determine outcomes
  if(is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }
  otcm_possibilities <- sort(unique(outcomes))

  # set output values
  for (i in seq_along(classifier)) {
    feature <- classifier[[i]][[1]] + 1
    orientation <- classifier[[i]][[2]]
    vote <- classifier[[i]][[3]]
    categorical <- classifier[[i]][[4]]
    split <- classifier[[i]][c(-1, -2, -3, -4)]

    # feature name
    output$feature[i] <- colnames(features)[[feature]]

    # vote
    output$vote[i] <- vote

    if (categorical == 0) {
      # orientation
      if (orientation == 1) {
        output$orientation[i] <- paste0(otcm_possibilities[[2]], "|", otcm_possibilities[[1]])
      } else {
        output$orientation[i] <- paste0(otcm_possibilities[[1]], "|", otcm_possibilities[[2]])
      }
      # split
      output$split[i] <- split
      output$left_categories[i] <- NA
    }
    if (categorical == 1) {
      # orientation
      output$orientation[i] <- paste0(otcm_possibilities[[1]], "|", otcm_possibilities[[2]])
      # categories
      temp_split <- rep(NA, length(split))
      feature_levels <- levels(addNA(factor(features[[feature]])))
      for (j in 1:length(split)) {
        temp_split[[j]] <- feature_levels[[split[[j]]]]
      }
      output$left_categories[i] <- paste(temp_split, collapse = "; ")
      output$split[i] <- NA
    }
  }

  return(output)
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




