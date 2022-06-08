// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// createTanimotoMatrix
NumericMatrix createTanimotoMatrix(LogicalMatrix fingerprints, NumericMatrix oldMatrix);
RcppExport SEXP _dartTools_createTanimotoMatrix(SEXP fingerprintsSEXP, SEXP oldMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalMatrix >::type fingerprints(fingerprintsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type oldMatrix(oldMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(createTanimotoMatrix(fingerprints, oldMatrix));
    return rcpp_result_gen;
END_RCPP
}
// createAsymmetricTanimotoMatrix
NumericVector createAsymmetricTanimotoMatrix(LogicalMatrix fingerprintsA, LogicalMatrix fingerprintsB);
RcppExport SEXP _dartTools_createAsymmetricTanimotoMatrix(SEXP fingerprintsASEXP, SEXP fingerprintsBSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalMatrix >::type fingerprintsA(fingerprintsASEXP);
    Rcpp::traits::input_parameter< LogicalMatrix >::type fingerprintsB(fingerprintsBSEXP);
    rcpp_result_gen = Rcpp::wrap(createAsymmetricTanimotoMatrix(fingerprintsA, fingerprintsB));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dartTools_createTanimotoMatrix", (DL_FUNC) &_dartTools_createTanimotoMatrix, 2},
    {"_dartTools_createAsymmetricTanimotoMatrix", (DL_FUNC) &_dartTools_createAsymmetricTanimotoMatrix, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dartTools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
