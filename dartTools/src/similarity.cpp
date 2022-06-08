#include <Rcpp.h>
using namespace Rcpp;

//' @title createTanimotoMatrix
//' @description
//' Function to calculate all pairwise Tanimoto similarities for one set of fingerprints
//' @param fingerprints Matrix with 0/1 values (rows: compounds, columns: fingerprint features)
//' @param oldMatrix Optional numeric matrix with precalculated similarities. The corresponding part of the
//' result matrix will be copied, rather than recalculated Logical, indicates wheter oldMatrix should be used
//' @return square matrix with Tanimoto similarities
//' @author Marvin Steijaert
//' @export
//' @examples a=matrix(sample(0:1,100,TRUE),10,10);b=matrix(1:4,2,2);createTanimotoMatrix(a,b)
//[[Rcpp::export]]
NumericMatrix createTanimotoMatrix(LogicalMatrix fingerprints,
		NumericMatrix oldMatrix = NumericMatrix() ){

	int nCompounds = fingerprints.nrow();
	NumericMatrix result(nCompounds,nCompounds);
	int nInOld = 0;
	if (oldMatrix.nrow() > 1){
		nInOld = oldMatrix.nrow();
	}
	LogicalVector cmp_a = fingerprints( 0, _);
	LogicalVector cmp_b = fingerprints( 0, _);

	int nFeatures = fingerprints.ncol();
	int counts_intersection;
	int counts_total;
	int cmpd_a_counts ;
	int cmpd_b_counts_vector [nCompounds];

	for (int j = 0; j < nCompounds; j++) {
		cmp_b = fingerprints(j,_);
		cmpd_b_counts_vector[j] = 0;
		for (int k = 0; k < nFeatures; k++) {
			if (cmp_b[k]) cmpd_b_counts_vector[j] += 1;
		}
	}

	// combined
	for (int i = 0; i < nInOld; i++) {
		for (int j = 0; j < nInOld; j++) {
			result(i,j) = oldMatrix(i,j);
		}
	}
	for (int i = nInOld; i < nCompounds; i++) {
		cmp_a = fingerprints(i,_);
		cmpd_a_counts = 0;
		for (int k = 0; k < nFeatures; k++) {
			if (cmp_a[k]) cmpd_a_counts += 1;
		}
		result(i,i) = 1.0; // diagonal: 1.0 by definition

		for (int j = 0; j < i; j++) {
			cmp_b = fingerprints(j,_);
			counts_intersection = 0 ;
			counts_total = cmpd_a_counts + cmpd_b_counts_vector[j] ;
			for (int k = 0; k < nFeatures; k++) {
				if (cmp_a[k] && cmp_b[k]) counts_intersection += 1;
			}
			if (counts_intersection == 0 && counts_total == 0){
				// by definition
				result(i,j) = 0.0;
			} else {
				// multiply with 1.0 to avoid integer division
				result(i,j) = (1.0*counts_intersection) /(counts_total - counts_intersection);
			}
			result(j,i) = result(i,j);
		}
	}

	return(result);
}

//' @title createAsymmetricTanimotoMatrix
//' @description
//' Function to calculate all pairwise Tanimoto similarities between two sets of fingerprints
//' @param fingerprintsA Matrix with 0/1 values (rows: compounds, columns: fingerprint features)
//' @param fingerprintsB Matrix with 0/1 values (rows: compounds, columns: fingerprint features)
//' @return matrix with Tanimoto similarities
//'
//' @author Marvin Steijaert
//[[Rcpp::export]]
NumericVector createAsymmetricTanimotoMatrix(LogicalMatrix fingerprintsA, LogicalMatrix fingerprintsB){

	int nCompoundsA = fingerprintsA.nrow();
	int nCompoundsB = fingerprintsB.nrow();
	NumericMatrix result(nCompoundsA,nCompoundsB);
	LogicalVector cmp_a = fingerprintsA( 0, _);
	LogicalVector cmp_b = fingerprintsB( 0, _);
	int nFeatures = fingerprintsA.ncol();
	int counts_intersection;
	int counts_total;
	int cmpd_a_counts ;
	int cmpd_b_counts_vector [nCompoundsB];

	for (int j = 0; j < nCompoundsB; j++) {
		cmp_b = fingerprintsB(j,_);
		cmpd_b_counts_vector[j] = 0;
		for (int k = 0; k < nFeatures; k++) {
			if (cmp_b[k]) cmpd_b_counts_vector[j] += 1;
		}
	}
	for (int i = 0; i < nCompoundsA; i++) {
		cmp_a = fingerprintsA(i,_);
		cmpd_a_counts = 0;
		for (int k = 0; k < nFeatures; k++) {
			if (cmp_a[k]) cmpd_a_counts += 1;
		}
		for (int j = 0; j < nCompoundsB; j++) {
			cmp_b = fingerprintsB(j,_);
			counts_intersection = 0 ;
			counts_total = cmpd_a_counts + cmpd_b_counts_vector[j] ;
			for (int k = 0; k < nFeatures; k++) {
				if (cmp_a[k] && cmp_b[k]) counts_intersection += 1;
			}
			if (counts_intersection == 0 && counts_total == 0){
				// by definition
				result(i,j) = 0.0;
			} else {
				// multiply with 1.0 to avoid integer division
				result(i,j) = (1.0*counts_intersection) /(counts_total - counts_intersection);
			}
		}
	}
	return(result);
}

