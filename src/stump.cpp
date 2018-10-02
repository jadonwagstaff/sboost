#include "stump.h"
#include <R.h>
#include <Rcpp.h>
#include <vector>
using namespace Rcpp;


// static variables
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



// Param: weights that are updated by a boosting algorithm
// Return: updates internal parameters of stump
void Stump::find_stump(const NumericVector& weights) {

  // CREATE VARIABLES
  // --------------------------------------------------------------------------------

  // categorical
  std::vector<double> positive, negative;
  unsigned int index = 0;

  // continous
  double positive_behind = 0, negative_behind = 0, positive_ahead = 0, negative_ahead = 0;
  double gain = 0;

  // both
  double na = 0, feature_gain = 0, feature_direction = 0, max_gain = 0;
  std::vector<double> feature_split;
  int feature_categorical = 0;


  // SEARCH THROUGH EACH FEATURE
  // --------------------------------------------------------------------------------
  for (int j = 0; j < features.ncol(); j++) {
    checkUserInterrupt();
    feature_split.clear();
    feature_gain = 0;
    na = 0;

    // IF CATEGORICAL FEATURE
    // --------------------------------------------------------------------------------
    if (categorical(j) > 1) {

      // clear variables
      feature_categorical = 1;
      positive.clear();
      negative.clear();

      // initialize vectors with length of number of categories
      for (int k = 0; k < categorical(j); k++) {
        negative.push_back(0);
        positive.push_back(0);
      }
      if (outcomes(ordered_index(0, j)) == 1) {
        positive[0] += weights(ordered_index(0, j));
      } else {
        negative[0] += weights(ordered_index(0, j));
      }
      index = 0; // index increments with new categories

      // calculate weighted positive and negative sums for each category
      for (int i = 1; i < features.nrow(); i++) {
        if (features(ordered_index(i - 1, j), j) != features(ordered_index(i, j), j)) {
          index++;
          // if the rest are missing, add 1/2 of remaining weight to "na"
          if (index == positive.size()) {
            while (i < features.nrow()) {
              na += 0.5 * weights(ordered_index(i, j));
              i++;
            }
            break;
          }
        }

        if (outcomes(ordered_index(i, j)) == 1) {
          positive[index] += weights(ordered_index(i, j));
        } else {
          negative[index] += weights(ordered_index(i, j));
        }
      }

      // add positive gains to feature split and calculate feature gain
      for (int k = 0; k < categorical(j); k++) {
        if (positive[k] > negative[k]) {
          feature_gain += positive[k];
          feature_split.push_back(k + 1);
        } else {
          feature_gain += negative[k];
        }
      }
      feature_gain += na;


      // if all categories are mostly positive or all mostly negative, gain is 0
      if (feature_split.size() == 0 || feature_split.size() == categorical(j)) {
        feature_gain =  0;
      }

      feature_direction = 1;

    // IF CONTINUOUS FEATURE
    // --------------------------------------------------------------------------------
    } else if (categorical(j) < 1) {

      // clear variables
      feature_categorical = 0;
      gain = 0;
      feature_split.push_back(0);
      positive_behind = 0;
      negative_behind = 0;
      positive_ahead = 0;
      negative_ahead = 0;

      // start with all instances as ahead
      for (int i = 0; i < features.nrow(); i++) {
        if (ISNAN(features(ordered_index(i, j), j))) {
          na += 0.5 * weights(ordered_index(i, j));
        } else if (outcomes(ordered_index(i, j)) == 1) {
          positive_ahead += weights(ordered_index(i, j));
        } else {
          negative_ahead += weights(ordered_index(i, j));
        }
      }

      // transfer low values to behind incrementally and test the split
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
          if (ISNAN(features(ordered_index(i, j), j))) {
            break;
          }

          // determine gain
          if (positive_ahead + negative_behind > negative_ahead + positive_behind) {
            gain = positive_ahead + negative_behind + na;
          } else {
            gain = negative_ahead + positive_behind + na;
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

    // TEST FEATURE
    // --------------------------------------------------------------------------------
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



// Param: current predictions which may have been updated by other stumps (same length as features)
// Return: predictions are changed to reflect the weighted influence of this stump
void Stump::update_predictions(NumericVector& predictions) const {
  bool in_split;
  if (is_categorical == 0) {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) += 0;
      } else if (features(i, feature) < split[0]) {
        predictions(i) += -1 * direction * vote;
      } else {
        predictions(i) += direction * vote;
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) += 0;
      } else {
        in_split = false;
        for (unsigned int j = 0; j < split.size(); j++) {
          if (features(i, feature) == split[j]) {
            predictions(i) += direction * vote;
            in_split = true;
            break;
          }
        }
        if (in_split == false) {
          predictions(i) += -1 * direction * vote;
        }
      }
    }
  }
}



// Param: vector of same length as features
// Return: votes for this stump
void Stump::new_predictions(NumericVector& predictions) const{
  if (is_categorical == 0) {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) = 0;
      } else if (features(i, feature) < split[0]) {
        predictions(i) = -1 * direction * vote;
      } else {
        predictions(i) = direction * vote;
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) = 0;
      } else {
        predictions(i) = -1 * vote;
        for (unsigned int j = 0; j < split.size(); j++) {
          if (features(i, feature) == split[j]) {
            predictions(i) = 1 * vote;
            break;
          }
        }
      }
    }
  }
}



// Param: vector of same length as features
// Return: unweighted predictions for this stump (-1 or 1 for each prediction)
void Stump::new_predictions_integer(NumericVector& predictions) const {
  if (is_categorical == 0) {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) = 0;
      } else if (features(i, feature) < split[0]) {
        predictions(i) = -1 * direction;
      } else {
        predictions(i) = direction;
      }
    }
  } else {
    for (int i = 0; i < features.nrow(); i++) {
      if (ISNAN(features(i, feature))) {
        predictions(i) = 0;
      } else {
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
}



// Param: predictions
// Return: contingencies based on accuracy of predictions
NumericVector Stump::get_contingencies(const NumericVector& predictions) const {
  // true_positive, false_negative, true_negative, false_positive
  NumericVector output(4);
  for (int i = 0; i < features.nrow(); i++) {
    if (outcomes(i) == 1) {
      if (predictions(i) >= 0) {
        // true_positive
        output(0)++;
      } else {
        // false_negative
        output(1)++;
      }
    } else {
      if (predictions(i) < 0) {
        // true_negative
        output(2)++;
      } else {
        // false_positive
        output(3)++;
      }
    }
  }
  return output;
}


double Stump::get_vote() const{
  return vote;
}


// Param: none
// Return: vector: feature, direction, vote, categorical, split...
NumericVector Stump::make_vector() const{

  NumericVector output = NumericVector::create(double(feature), double(direction), double(vote), double(is_categorical));
  for (unsigned int i = 0; i < split.size(); i++) {
    output.push_back(split[i]);
  }
  return(output);
}





