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

  void find_stump(const NumericVector& weights);
  void set_vote(double v);

  void update_predictions(NumericVector& predictions) const;

  int get_feature() const;
  int get_direction() const;
  double get_vote() const;
  int get_categorical() const;
  double get_split() const;
  double get_split(int index) const;
  int split_size() const;
  NumericVector make_vector() const;

  int get_prediction(double value) const;

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
