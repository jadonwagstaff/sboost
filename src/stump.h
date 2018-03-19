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
  static void populate_data(const NumericMatrix& f, const NumericVector& o, const NumericMatrix& oi, const NumericVector& c);
  static void populate_data(const NumericMatrix& f, const NumericVector& o);
  static void populate_data(const NumericMatrix& f);

  void find_stump(const NumericVector& weights);
  void set_vote(double v);

  double get_vote() const;
  void update_predictions(NumericVector& predictions) const;
  void new_predictions(NumericVector& predictions) const;
  NumericVector get_contingencies(const NumericVector& predictions) const;
  NumericVector make_vector() const;

private:
  static NumericMatrix features;
  static NumericVector outcomes;
  static NumericMatrix ordered_index;
  static NumericVector categorical;

  int feature;
  int direction;
  double vote;
  int is_categorical;
  std::vector<double> split;
};


#endif
