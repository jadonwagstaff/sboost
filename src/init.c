// generated using tools::package_native_routine_registration_skeleton(".", character_only = FALSE)

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _sboost_adaboost(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _sboost_assess(SEXP, SEXP, SEXP);
extern SEXP _sboost_predict(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_sboost_adaboost", (DL_FUNC) &_sboost_adaboost, 5},
  {"_sboost_assess",   (DL_FUNC) &_sboost_assess,   3},
  {"_sboost_predict",  (DL_FUNC) &_sboost_predict,  2},
  {NULL, NULL, 0}
};

void R_init_sboost(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
