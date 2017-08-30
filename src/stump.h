#ifndef STUMP_H
#define STUMP_H

#include <Rcpp.h>
using namespace Rcpp;

// find_stump
// Param: feature matrix, corresponding outcomes, weights for each row of feature matrix
// Return: classifier as a stump without a vote, and updates weights vector
// [[Rcpp::export]]
NumericVector find_stump(NumericMatrix features, NumericMatrix outcome_index, NumericVector outcomes, NumericVector weights);

#endif
