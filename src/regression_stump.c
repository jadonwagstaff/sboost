#include <R.h>
#include <Rinternals.h>

SEXP regression_stump_(SEXP Rpredictor, SEXP Rorder, SEXP Rresponse) {
  double *predictor, *response, *weights;
  int *order;
  predictor = REAL(Rpredictor);
  order = INTEGER(Rorder);
  response = REAL(Rresponse);

  int n = length(Rpredictor);
  double na = 0;
  double loss = 0;
  double split = 0;
  double best_loss = 0;
  double best_mean_ahead = 0;
  double best_mean_behind = 0;
  int count = 0;
  double mean_behind = 0;
  double sse_behind = 0;
  double mean_ahead[n];
  double sse_ahead[n];
  double temp_mean = 0;

  // find running mean and sse for all instances ahead in descending order
  mean_ahead[n-1] = response[order[n-1]];
  sse_ahead[n-1] = 0;
  for (int i = n-1; i > 0; i--) {
    mean_ahead[i-1] = mean_ahead[i] + (response[order[i-1]] - mean_ahead[i]) / (n - i + 1);
    sse_ahead[i-1] = sse_ahead[i] + (response[order[i-1]] - mean_ahead[i]) * (response[order[i-1]] - mean_ahead[i-1]);
  }

  // find running mean and sse for all instances behind in ascending order and test the split
  mean_behind = response[order[0]];
  sse_behind = 0;
  best_loss = sse_ahead[0];
  for (int i = 1; i < n; i++) {

    // find loss if this and the last sample were different for this predictor and compare to best loss
    if (predictor[order[i - 1]] != predictor[order[i]] && sse_behind + sse_ahead[i] < best_loss) {
      best_loss = sse_behind + sse_ahead[i];
      split = (predictor[order[i - 1]] + predictor[order[i]]) / 2;
      best_mean_ahead = mean_ahead[i];
      best_mean_behind = mean_behind;
    }

    // Update data for behind potential split
    temp_mean = mean_behind;
    mean_behind = mean_behind + (response[order[i]] - mean_behind) / (i + 1);
    sse_behind = sse_behind + (response[order[i]] - temp_mean) * (response[order[i]] - mean_behind);
  }

  SEXP result = PROTECT(allocVector(REALSXP, 4));
  REAL(result)[0] = best_loss;
  REAL(result)[1] = split;
  REAL(result)[2] = best_mean_behind;
  REAL(result)[3] = best_mean_ahead;
  UNPROTECT(1);
  return result;
}

