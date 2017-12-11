#ifndef STUMP_H
#define STUMP_H

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

class Stump
{
public:
  Stump();
  Stump(NumericVector stump_in);

  void find_stump(NumericMatrix& features, NumericMatrix& outcome_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical);
  void set_feature(int f);
  void set_direction(int d);
  void set_vote(double v);
  void set_categorical(int c);
  void add_split(double s);

  int get_feature() const;
  int get_direction() const;
  double get_vote() const;
  int get_categorical() const;
  double get_split() const;
  double get_split(int index) const;
  int split_size() const;
  NumericVector make_vector() const;

private:
  int feature;
  int direction;
  double vote;
  int is_categorical;
  std::vector<double> split;

};


#endif
