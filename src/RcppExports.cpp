// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// adaboost_class
List adaboost_class(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& categorical, int iterations);
RcppExport SEXP _sboost_adaboost_class(SEXP featuresSEXP, SEXP ordered_indexSEXP, SEXP outcomesSEXP, SEXP categoricalSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type ordered_index(ordered_indexSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type outcomes(outcomesSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type categorical(categoricalSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(adaboost_class(features, ordered_index, outcomes, categorical, iterations));
    return rcpp_result_gen;
END_RCPP
}
// adaboost_regress
List adaboost_regress(NumericMatrix& features, NumericMatrix& ordered_index, NumericVector& outcomes, NumericVector& categorical, int iterations);
RcppExport SEXP _sboost_adaboost_regress(SEXP featuresSEXP, SEXP ordered_indexSEXP, SEXP outcomesSEXP, SEXP categoricalSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type ordered_index(ordered_indexSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type outcomes(outcomesSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type categorical(categoricalSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(adaboost_regress(features, ordered_index, outcomes, categorical, iterations));
    return rcpp_result_gen;
END_RCPP
}
// find_classifier_contingency
NumericMatrix find_classifier_contingency(NumericMatrix& features, NumericVector& outcomes, List& classifier);
RcppExport SEXP _sboost_find_classifier_contingency(SEXP featuresSEXP, SEXP outcomesSEXP, SEXP classifierSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type features(featuresSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type outcomes(outcomesSEXP);
    Rcpp::traits::input_parameter< List& >::type classifier(classifierSEXP);
    rcpp_result_gen = Rcpp::wrap(find_classifier_contingency(features, outcomes, classifier));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sboost_adaboost_class", (DL_FUNC) &_sboost_adaboost_class, 5},
    {"_sboost_adaboost_regress", (DL_FUNC) &_sboost_adaboost_regress, 5},
    {"_sboost_find_classifier_contingency", (DL_FUNC) &_sboost_find_classifier_contingency, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_sboost(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
