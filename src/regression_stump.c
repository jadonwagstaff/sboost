#include <R.h>
#include <Rinternals.h>

SEXP regression_stump_(SEXP Rpredictor, SEXP Rorder, SEXP Rresponse) {
  double *predictor, *response;
  int *order;
  predictor = REAL(Rpredictor);
  order = INTEGER(Rorder);
  response = REAL(Rresponse);

  int n = length(Rpredictor);
  double split = 0;
  double best_sse = 0;
  double best_beta0_ahead = 0;
  double best_beta0_behind = 0;
  double best_beta1_ahead = 0;
  double best_beta1_behind = 0;
  double temp_ybar = 0;
  double temp_xbar = 0;

  double sse_behind = 0;
  double ybar_behind = 0;
  double sy_behind = 0;
  double xbar_behind = 0;
  double sx_behind = 0;
  double sxy_behind = 0;

  double sse_ahead = 0;
  double ybar_ahead[n];
  double sy_ahead[n];
  double xbar_ahead[n];
  double sx_ahead[n];
  double sxy_ahead[n];

  // find running ybar, sy, xbar, sx, and sxy for all instances ahead in descending order
  ybar_ahead[n-1] = response[order[n-1]];
  sy_ahead[n-1] = 0;
  xbar_ahead[n-1] = predictor[order[n-1]];
  sx_ahead[n-1] = 0;
  sxy_ahead[n-1] = 0;
  for (int i = n-1; i > 0; i--) {
    ybar_ahead[i-1] = ybar_ahead[i] + (response[order[i-1]] - ybar_ahead[i]) / (n - i + 1);
    sy_ahead[i-1] = sy_ahead[i] + (response[order[i-1]] - ybar_ahead[i]) * (response[order[i-1]] - ybar_ahead[i-1]);
    xbar_ahead[i-1] = xbar_ahead[i] + (predictor[order[i-1]] - xbar_ahead[i]) / (n - i + 1);
    sx_ahead[i-1] = sx_ahead[i] + (predictor[order[i-1]] - xbar_ahead[i]) * (predictor[order[i-1]] - xbar_ahead[i-1]);
    sxy_ahead[i-1] = sxy_ahead[i] + (predictor[order[i-1]] - xbar_ahead[i]) * (response[order[i-1]] - ybar_ahead[i-1]);
  }

  // find running ybar, sy, xbar, sx, and sxy for all instances behind in ascending order and test the split
  ybar_behind = response[order[0]];
  sy_behind = 0;
  xbar_behind = predictor[order[0]];
  sx_behind = 0;
  sxy_behind = 0;
  if (sx_ahead[0] == 0) {
    best_sse = sy_ahead[0];
  } else{
    best_sse = sy_ahead[0] - (sxy_ahead[0] * sxy_ahead[0]) / sx_ahead[0];
  }
  for (int i = 1; i < n; i++) {

    // find loss if this and the last sample were different for this predictor and compare to best loss
    if (predictor[order[i - 1]] != predictor[order[i]] && sx_ahead[i] != 0 && sx_behind != 0) {
      if (sx_ahead[i] == 0) {
        sse_ahead = sy_ahead[0];
      } else{
        sse_ahead = sy_ahead[i] - (sxy_ahead[i] * sxy_ahead[i]) / sx_ahead[i];
      }
      if (sx_behind == 0) {
        sse_behind = sy_behind;
      } else {
        sse_behind = sy_behind - (sxy_behind * sxy_behind) / sx_behind;
      }
      if (sse_behind + sse_ahead < best_sse) {
        best_sse = sse_behind + sse_ahead;
        split = (predictor[order[i - 1]] + predictor[order[i]]) / 2;
        best_beta1_ahead = sxy_ahead[i] / sx_ahead[i];
        best_beta1_behind = sxy_behind / sx_behind;
        best_beta0_ahead = ybar_ahead[i] - best_beta1_ahead * xbar_ahead[i];
        best_beta0_behind = ybar_behind - best_beta1_behind * xbar_behind;
      }
    }

    // Update data for behind potential split
    temp_ybar = ybar_behind;
    ybar_behind = ybar_behind + (response[order[i]] - ybar_behind) / (i + 1);
    sy_behind = sy_behind + (response[order[i]] - temp_ybar) * (response[order[i]] - ybar_behind);
    temp_xbar = xbar_behind;
    xbar_behind = xbar_behind + (predictor[order[i]] - xbar_behind) / (i + 1);
    sx_behind = sx_behind + (predictor[order[i]] - temp_xbar) * (predictor[order[i]] - xbar_behind);
    sxy_behind = sxy_behind + (predictor[order[i]] - xbar_behind) * (response[order[i]] - temp_ybar);
  }

  SEXP result = PROTECT(allocVector(REALSXP, 6));
  REAL(result)[0] = best_sse;
  REAL(result)[1] = split;
  REAL(result)[2] = best_beta0_behind;
  REAL(result)[3] = best_beta1_behind;
  REAL(result)[4] = best_beta0_ahead;
  REAL(result)[5] = best_beta1_ahead;
  UNPROTECT(1);
  return result;
}

