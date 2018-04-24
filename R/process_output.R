# --------------------------------------------------------------------------------
# PREPARES CLASSIFIER OUTPUT
process_classifier_output <- function(classifier, features, outcomes) {

  # create output data frame
  output <- data.frame(matrix(ncol = 6, nrow = length(classifier)))
  colnames(output) <- c("stump", "feature", "vote", "orientation", "split", "left_categories")

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

    # stump
    output$stump[i] <- i

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
