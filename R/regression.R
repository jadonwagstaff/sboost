regression_stump <- function(p, o, r) .Call(regression_stump_, p, o, r)

regression <- function(features, outcomes, iterations) {

  ordered_index <- matrix(NA, nrow = nrow(features), ncol = ncol(features))
  for (i in 1:ncol(features)) {
    ordered_index[, i] <- order(features[, i]) - 1L
  }
  weights <- rep(1 / nrow(features), nrow(features))
  #outcomes <- ifelse(outcomes == 1, 1, -1)

  classifier <- data.frame(
    feature = rep(0, iterations),
    vote = rep(0, iterations),
    split = rep(0, iterations),
    mean_behind = rep(0, iterations),
    mean_ahead = rep(0, iterations)
  )

  candidates <- data.frame(
    error = rep(0, ncol(features)),
    split = rep(0, ncol(features)),
    mean_behind = rep(0, ncol(features)),
    mean_ahead = rep(0, ncol(features))
  )

  for (i in 1:iterations) {
    selection <- sample(1L:nrow(features), nrow(features), replace = TRUE, prob = weights)
    selection <- sort(selection)

    for (j in 1:ncol(features)) {
      candidates[j,] <- regression_stump(
        as.numeric(features[, j]),
        ordered_index[selection, j],
        as.numeric(outcomes)
      )
    }

    stump_feature <- which.min(candidates$error)
    stump_split <- candidates$split[stump_feature]
    stump_mean_behind <- candidates$mean_behind[stump_feature]
    stump_mean_ahead <- candidates$mean_ahead[stump_feature]

    predictions <- ifelse(features[, stump_feature] < stump_split, stump_mean_behind, stump_mean_ahead)
    losses <- (outcomes - predictions)^2
    losses <- losses / max(losses)
    ave_loss <- sum(losses * weights)
    beta = ave_loss / (1 - ave_loss)
    vote = log(1 / beta)

    weights = weights * beta^(1 - losses)
    weights = weights / sum(weights)

    if (ave_loss >= 0.5) {
      classifier <- classifier[1:(i - 1),]
      break
    }

    classifier[i,] <- c(stump_feature, vote, stump_split, stump_mean_behind, stump_mean_ahead)
  }

  return(classifier)
}
