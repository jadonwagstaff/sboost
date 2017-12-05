#ifndef STUMP_H
#define STUMP_H

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

class Stump
{
public:
  Stump();
  Stump(int feature_in, double split_in, int direction_in, double vote_in);

  // Stump initiator function
  // Parameters: feature matrix, corresponding outcomes, weights for each row of feature matrix
  void find_stump(NumericMatrix& features, NumericMatrix& outcome_index, NumericVector& outcomes, NumericVector& weights);
  void set_vote(double v);

  int get_feature() const;
  double get_split() const;
  int get_direction() const;
  double get_vote() const;

private:
  int feature;
  double split;
  int direction;
  double vote;

};


#endif
