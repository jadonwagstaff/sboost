#include "regression_stump.h"
#include <Rcpp.h>
using namespace Rcpp;

Regression_Stump::Regression_Stump() : Stump() {
  left = 0;
  right = 0;
}


Regression_Stump::Regression_Stump(NumericVector stump_in) : Stump() {
  set_feature(stump_in(0));
  set_direction(stump_in(1));
  set_vote(stump_in(2));
  left = stump_in(3);
  right = stump_in(4);
  set_categorical(stump_in(5));
  for (int i = 6; i < stump_in.size(); i++) {
    add_split(stump_in(i));
  }
}


void Regression_Stump::find_stump(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical) {

}


double Regression_Stump::get_left() const {
  return left;
}


double Regression_Stump::get_right() const {
  return right;
}


NumericVector Regression_Stump::make_vector() const{
  NumericVector output = NumericVector::create(double(get_feature()), double(get_direction()), double(get_vote()), left, right, double(get_categorical()));
  for (int i = 0; i < split_size(); i++) {
    output.push_back(get_split(i));
  }
  return(output);
}





