regression_stump <- function(p, o, r) .Call(regression_stump_, p, o, r)

regression <- function(features, outcomes, iterations) {

  ordered_index <- matrix(NA, nrow = nrow(features), ncol = ncol(features))
  for (i in 1:ncol(features)) {
    ordered_index[, i] <- order(features[, i]) - 1L
  }
  weights <- rep(1 / nrow(features), nrow(features))

  model <- data.frame(
    feature = rep(0, iterations),
    vote = rep(0, iterations),
    split = rep(0, iterations),
    beta0_behind = rep(0, iterations),
    beta1_behind = rep(0, iterations),
    beta0_ahead = rep(0, iterations),
    beta1_ahead = rep(0, iterations)
  )

  candidates <- data.frame(
    error = rep(0, ncol(features)),
    split = rep(0, ncol(features)),
    beta0_behind = rep(0, ncol(features)),
    beta1_behind = rep(0, ncol(features)),
    beta0_ahead = rep(0, ncol(features)),
    beta1_ahead = rep(0, ncol(features))
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
    stump_beta0_behind <- candidates$beta0_behind[stump_feature]
    stump_beta1_behind <- candidates$beta1_behind[stump_feature]
    stump_beta0_ahead <- candidates$beta0_ahead[stump_feature]
    stump_beta1_ahead <- candidates$beta1_ahead[stump_feature]

    predictions <- ifelse(features[, stump_feature] < stump_split,
                          features[, stump_feature] * stump_beta1_behind + stump_beta0_behind,
                          features[, stump_feature] * stump_beta1_ahead + stump_beta0_ahead)
    losses <- (outcomes - predictions)^2
    if (max(losses) != 0) losses <- losses / max(losses)
    ave_loss <- sum(losses * weights)
    beta = ave_loss / (1 - ave_loss)
    vote = log(1 / beta)

    weights = weights * beta^(1 - losses)
    weights = weights / sum(weights)

    if (ave_loss >= 0.5) {
      model <- model[1:(i - 1),]
      break
    }

    model[i,] <- c(stump_feature, vote, stump_split,
                        stump_beta0_behind, stump_beta1_behind,
                        stump_beta0_ahead, stump_beta1_ahead)
  }

  return(model)
}
