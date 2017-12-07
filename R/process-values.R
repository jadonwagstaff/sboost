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
# TESTS AND PREPARES CLASSIFIER INPUT
process_classifier <- function(classifier, features, outcomes) {

  if (!is.list(classifier)) {
    message("ERROR: Classifier must be a list.")
    return(NULL)
  }

  new_classifier = list()

  # determine outcomes
  if(is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }
  otcm_possibilities <- sort(unique(outcomes))

  for (i in seq_along(classifier)) {
    # feature, direction, vote, split
    if (length(classifier[[i]]$feature) != 1 ||
        length(classifier[[i]]$direction) != 1 ||
        length(classifier[[i]]$vote) != 1 ||
        length(classifier[[i]]$categorical) != 1 ||
        length(classifier[[i]]$split) < 1) {
      message("ERROR: Classifier not valid.")
      return(NULL)
    }
    feature <- classifier[[i]]$feature
    direction <- classifier[[i]]$direction
    categorical <- classifier[[i]]$categorical
    vote <- classifier[[i]]$vote
    split <- classifier[[i]]$split

    # Change feature
    feature <- match(feature, colnames(features))[[1]] - 1

    # Change direction
    if (categorical == TRUE) {
      direction <- 1
    } else {
      if (substr(direction, 1, 2) == "->") {
        direction <- substr(direction, 4, nchar(direction))
        if (match(otcm_possibilities, direction)[[1]] == 1) {
          direction <- 1
        } else {
          direction <- -1
        }
      } else {
        direction <- substr(direction, 4, nchar(direction))
        if (match(direction, otcm_possibilities)[[1]] == 1) {
          direction <- -1
        } else {
          direction <- 1
        }
      }
    }

    # Change categorical
    categorical <- as.numeric(categorical)

    # Change split
    if (categorical == 1) {
      feature_levels <- levels(factor(features[[feature + 1]]))
      for (j in seq_along(split)) {
        split[[j]] <- match(split[[j]], feature_levels)
      }
    }

    new_classifier[[i]] <- as.numeric(c(feature, direction, vote, categorical, split))

  }

  return(new_classifier)
}


# --------------------------------------------------------------------------------
# PREPARES CLASSIFIER OUTPUT
prepare_classifier <- function(classifier, features, outcomes) {

  # determine outcomes
  if(is.data.frame(outcomes)) {
    outcomes <- outcomes[[1]]
  }
  otcm_possibilities <- sort(unique(outcomes))

  for (i in seq_along(classifier)) {
    feature <- classifier[[i]][[1]]
    direction <- classifier[[i]][[2]]
    vote <- classifier[[i]][[3]]
    categorical <- classifier[[i]][[4]]
    split <- classifier[[i]][c(-1, -2, -3, -4)]
    classifier[[i]] <- list()

    # Change stump name
    names(classifier)[[i]] <- i

    # Change feature name
    classifier[[i]]$feature <- colnames(features)[[feature]]

    # Change direction
    if (categorical == 1) {
      classifier[[i]]$direction <- paste0("split <- ", otcm_possibilities[[1]])
    } else if (direction == 1) {
      classifier[[i]]$direction <- paste0("-> ", otcm_possibilities[[1]])
    } else {
      classifier[[i]]$direction <- paste0("<- ", otcm_possibilities[[1]])
    }

    # Change categorical value
    if (categorical == 1) {
      classifier[[i]]$categorical <- TRUE
    } else {
      classifier[[i]]$categorical <- FALSE
    }

    # Change vote
    classifier[[i]]$vote <- vote

    # change split
    if (categorical == 1) {
      classifier[[i]]$split <- rep(0, length(split))
      feature_levels <- levels(factor(features[[feature]]))
      for (j in 1:length(classifier[[i]]$split)) {
        classifier[[i]]$split[[j]] <- feature_levels[[split[[j]]]]
      }
    } else {
      classifier[[i]]$split <- split
    }
  }

  return(classifier)
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




