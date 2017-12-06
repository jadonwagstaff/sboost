# --------------------------------------------------------------------------------
# TESTS AND PREPARES FEATURES
process_features <- function(features) {

  if (!is.data.frame(features) & !is.matrix(features)) {
    message("ERROR: Features must be data frame or matrix.")
    return(NULL)
  }

  for (i in seq_along(features)) {
    if (is.logical(features[[i]])) {
      features[[i]] <- as.numeric(features[[i]])
    } else if (is.character(features[[i]])) {
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
# FIND CATEGORICAL VECTOR
find_categorical <- function(features) {
  categorical <- rep(0, ncol(features))

  for (i in seq_along(features)) {
    if (is.logical(features[[i]]) || is.character(features[[i]]) || is.factor(features[[i]])) {
      categorical[[i]] <- 1
    }
  }

  return(categorical)
}




