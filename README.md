# sboost
Machine learning boosting decision stumps R package.

## Installation
R and the devtools package is required to install the sboost package.

To install devtools on R run:

```
install.packages("devtools")
```

After devtools is installed, to install the sboost package on R run:

```
devtools::install_github("jadonwagstaff/sboost")
```

## Functions

*sboost* - Main machine learning algorithm, uses categorical or continuous features to build a classifier that predicts a binary outcome. See documentation in R for instructions on use (?sboost::sboost) and see [jadonwagstaff.github.io/sboost](https://jadonwagstaff.github.io/sboost.html) for more information on how the algorithm works.

*validate* - Uses k-fold cross validation on a training set to validate the classifier.

*assessment* - Shows performance of a classifier on a testing or training set.

*predictions* - Outputs predictions of a classifier on feature vectors.

## Author
Jadon Wagstaff

## Licence
MIT
