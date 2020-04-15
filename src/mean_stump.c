#include <R.h>
#include <Rinternals.h>

SEXP mean_stump_(SEXP Rpredictor, SEXP Rorder, SEXP Rresponse) {
  double *predictor, *response;
  int *order;
  predictor = REAL(Rpredictor);
  order = INTEGER(Rorder);
  response = REAL(Rresponse);

  int n = length(Rpredictor);
  double split = 0;
  double best_sse = 0;
  double best_ybar_ahead = 0;
  double best_ybar_behind = 0;
  double temp_ybar = 0;

  double ybar_behind = 0;
  double sy_behind = 0;

  double ybar_ahead[n];
  double sy_ahead[n];

  // find running mean and sy for all instances ahead in descending order
  ybar_ahead[n-1] = response[order[n-1]];
  sy_ahead[n-1] = 0;
  for (int i = n-1; i > 0; i--) {
    ybar_ahead[i-1] = ybar_ahead[i] + (response[order[i-1]] - ybar_ahead[i]) / (n - i + 1);
    sy_ahead[i-1] = sy_ahead[i] + (response[order[i-1]] - ybar_ahead[i]) * (response[order[i-1]] - ybar_ahead[i-1]);
  }

  // find running mean and sy for all instances behind in ascending order and test the split
  ybar_behind = response[order[0]];
  sy_behind = 0;
  best_sse = sy_ahead[0];
  for (int i = 1; i < n; i++) {

    // find loss if this and the last sample were different for this predictor and compare to best loss
    if (predictor[order[i - 1]] != predictor[order[i]] && sy_behind + sy_ahead[i] < best_sse) {
      best_sse = sy_behind + sy_ahead[i];
      split = (predictor[order[i - 1]] + predictor[order[i]]) / 2;
      best_ybar_ahead = ybar_ahead[i];
      best_ybar_behind = ybar_behind;
    }

    // Update data for behind potential split
    temp_ybar = ybar_behind;
    ybar_behind = ybar_behind + (response[order[i]] - ybar_behind) / (i + 1);
    sy_behind = sy_behind + (response[order[i]] - temp_ybar) * (response[order[i]] - ybar_behind);
  }

  SEXP result = PROTECT(allocVector(REALSXP, 4));
  REAL(result)[0] = best_sse;
  REAL(result)[1] = split;
  REAL(result)[2] = best_ybar_behind;
  REAL(result)[3] = best_ybar_ahead;
  UNPROTECT(1);
  return result;
}
