# --------------------------------------------------------------------------------
# TESTS AND PREPARES FEATURES
process_features <- function(features) {

  if (!is.data.frame(features)) {
    message("ERROR: Features must be data frame.")
    return(NULL)
  }

  for (i in seq_along(features)) {
    if (!is.numeric(features[[i]])) {
      message("ERROR: All data.frame columns must be numeric.")
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

  return(outcomes)
}
