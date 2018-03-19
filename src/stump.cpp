#include "stump.h"
#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

NumericMatrix Stump::features = NumericMatrix();
NumericVector Stump::outcomes = NumericVector();

NumericMatrix Stump::ordered_index = NumericMatrix();
NumericVector Stump::categorical = NumericVector();


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


void Stump::populate_data(const NumericMatrix& f, const NumericVector& o, const NumericMatrix& oi, const NumericVector& c) {
  features = f;
  outcomes = o;

  ordered_index = oi;
  categorical = c;
}


void Stump::populate_data(const NumericMatrix& f, const NumericVector& o) {
  features = f;
  outcomes = o;
}


void Stump::populate_data(const NumericMatrix& f) {
  features = f;
}


void Stump::find_stump(const NumericVector& weights) {

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
      gain = 0;
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

          // deal with NA: randomly distribute over best split
          if (std::isnan(features(ordered_index(i, j), j))) {
            if (gain != 0) {
              for (int newi = i; newi < features.nrow(); newi++) {
                if (rand() % 2 == 0) {
                  if (outcomes(ordered_index(newi - 1, j)) == 1) {
                    if (feature_direction != 1) {
                      feature_gain += weights(ordered_index(newi - 1, j));
                    }
                  } else {
                    if (feature_direction == 1) {
                      feature_gain += weights(ordered_index(newi - 1, j));
                    }
                  }
                } else {
                  if (outcomes(ordered_index(newi - 1, j)) == 1) {
                    if (feature_direction == 1) {
                      feature_gain += weights(ordered_index(newi - 1, j));
                    }
                  } else {
                    if (feature_direction != 1) {
                      feature_gain += weights(ordered_index(newi - 1, j));
                    }
                  }
                }
              }
            }
            break;
          }

          // determine gain
          if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
            gain = positive_ahead + negative_behind;
          } else {
            gain = negative_ahead + positive_behind;
          }

          // see if gain is best
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


void Stump::update_predictions(NumericVector& predictions) const {
  bool in_split;
  if (is_categorical == 0) {
    for (int i = 0; i < features.nrow(); i++) {
      if (features(i, feature) < split[0]) {
        predictions(i) += -1 * direction * vote;
      } else {
        predictions(i) += direction * vote;
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      in_split = false;
      for (unsigned int j = 0; j < split.size(); j++) {
        if (features(i, feature) == split[j]) {
          predictions(i) += vote;
          in_split = true;
          break;
        }
      }
      if (in_split == false) {
        predictions(i) += -1 * vote;
      }
    }
  }
}


void Stump::new_predictions(NumericVector& predictions) const {
  if (is_categorical == 0) {
    for (int i = 0; i < features.nrow(); i++) {
      if (features(i, feature) < split[0]) {
        predictions(i) = -1 * direction;
      } else {
        predictions(i) = direction;
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      predictions(i) = -1;
      for (unsigned int j = 0; j < split.size(); j++) {
        if (features(i, feature) == split[j]) {
          predictions(i) = 1;
          break;
        }
      }
    }
  }
}


NumericVector Stump::get_contingencies(const NumericVector& predictions) const {
  // true_positive, false_negative, true_negative, false_positive
  NumericVector output(4);
  for (int i = 0; i < features.nrow(); i++) {
    if (outcomes(i) == 1) {
      if (predictions(i) >= 0) {
        output(0)++;
      } else {
        output(1)++;
      }
    } else {
      if (predictions(i) < 0) {
        output(2)++;
      } else {
        output(3)++;
      }
    }
  }
  return output;
}


double Stump::get_vote() const{
  return vote;
}


NumericVector Stump::make_vector() const{
  NumericVector output = NumericVector::create(double(feature), double(direction), double(vote), double(is_categorical));
  for (unsigned int i = 0; i < split.size(); i++) {
    output.push_back(split[i]);
  }
  return(output);
}





