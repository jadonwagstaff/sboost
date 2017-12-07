#include "stump.h"
#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

Stump::Stump() {
  feature = 0;
  direction = 0;
  vote = 0;
  is_categorical = 0;
  split.push_back(0);
}

Stump::Stump(NumericVector stump_in) {
  feature = stump_in(0);
  direction = stump_in(1);
  vote = stump_in(2);
  is_categorical = stump_in(3);
  for (int i = 4; i < stump_in.size(); i++) {
    split.push_back(stump_in(i));
  }
}


void Stump::find_stump(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& weights, NumericVector& categorical) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------

  // categorical
  std::vector<double> positive, negative;
  int index = 0;

  // continous
  double positive_behind = 0, negative_behind = 0, positive_ahead = 0, negative_ahead = 0;
  double gain = 0;

  // both
  double feature_gain = 0, feature_direction = 0, max_gain = 0;
  std::vector<double> feature_split;
  int feature_categorical = 0;



  for (int j = 0; j < features.ncol(); j++) {
    feature_split.clear();
    feature_gain = 0;

    if (categorical(j) > 1) {
      // Categorical
      feature_categorical = 1;
      positive.clear();
      negative.clear();
      for (int k = 0; k < categorical(j); k++) {
        negative.push_back(0);
        positive.push_back(0);
      }
      if (outcomes(ordered_index(0, j)) == 1) {
        positive[0] += weights(ordered_index(0, j));
      } else {
        negative[0] += weights(ordered_index(0, j));
      }
      index = 0;


      for (int i = 1; i < features.nrow(); i++) {
        if (features(ordered_index(i - 1, j), j) != features(ordered_index(i, j), j)) {
          index++;
        }
        if (outcomes(ordered_index(i, j)) == 1) {
          positive[index] += weights(ordered_index(i, j));
        } else {
          negative[index] += weights(ordered_index(i, j));
        }
      }

      for (int k = 0; k < categorical(j); k++) {
        if (positive[k] > negative[k]) {
          feature_gain += positive[k];
          feature_split.push_back(k + 1);
        } else {
          feature_gain += negative[k];
        }
      }
      if (feature_split.size() == 0 || feature_split.size() == categorical(j)) {
        feature_gain =  0;
      }
      feature_direction = 1;


    } else if (categorical(j) < 1) {

      // Continuous
      feature_categorical = 0;
      feature_split.push_back(0);
      positive_behind = 0;
      negative_behind = 0;
      positive_ahead = 0;
      negative_ahead = 0;
      for (int i = 0; i < features.nrow(); i++) {
        if (outcomes(ordered_index(i, j)) == 1) {
          positive_ahead += weights(ordered_index(i, j));
        } else {
          negative_ahead += weights(ordered_index(i, j));
        }
      }
      // find best gain for this features
      for (int i = 1; i < features.nrow(); i++) {
        // update counting variables
        if (outcomes(ordered_index(i - 1, j)) == 1) {
          positive_behind += weights(ordered_index(i - 1, j));
          positive_ahead += -1 * weights(ordered_index(i - 1, j));
        } else {
          negative_behind += weights(ordered_index(i - 1, j));
          negative_ahead += -1 * weights(ordered_index(i - 1, j));
        }

        // find gain if this and the last sample were different for this feature, compare to feature gain
        if (features(ordered_index(i - 1, j), j) != features(ordered_index(i, j), j) ) {

          if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
            gain = positive_ahead + negative_behind;
          } else {
            gain = negative_ahead + positive_behind;
          }


          if (gain > feature_gain) {
            feature_gain = gain;
            feature_split[0] = (features(ordered_index(i - 1, j), j) + features(ordered_index(i, j), j)) / 2;
            if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
              feature_direction = 1;
            } else {
              feature_direction = -1;
            }
          }
        }
      }
    }

    // if this features gain is the best, update maxGain and best Feature
    if (feature_gain > max_gain) {
      max_gain = feature_gain;
      feature = j;
      direction = feature_direction;
      is_categorical = feature_categorical;
      split = feature_split;
    }
  }

}


void Stump::set_vote(double v) {
  vote = v;
}


int Stump::get_feature() const {
  return feature;
}


int Stump::get_direction() const{
  return direction;
}


double Stump::get_vote() const{
  return vote;
}

int Stump::get_categorical() const {
  return is_categorical;
}


double Stump::get_split() const {
  return split[0];
}

double Stump::get_split(int index) const {
  return split[index];
}

int Stump::split_size() const {
  return split.size();
}


NumericVector Stump::make_vector() const{
  NumericVector output = NumericVector::create(double(feature), double(direction), double(vote), double(is_categorical));
  for (int i = 0; i < split.size(); i++) {
    output.push_back(split[i]);
  }
  return(output);
}





