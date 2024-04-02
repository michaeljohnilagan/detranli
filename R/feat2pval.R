#' Convert features to p-values
#' 
#' Compute a p-value per observed feature vector, with respect to its null 
#' distribution.
#' 
#' When there are issues computing feature-space covariances, p-value is \code{NA}.
#' 
#' @param x feature-space data (matrix or dataframe) of interest.
#' @param nulldist feature-space data (matrix or dataframe) for the empirical null distribution.
#' @param feat_idvals vector of ideal values. Length must be equal to the number of columns of \code{x} as well as \code{nulldist}.
#' 
#' @return Vector of p-values.
#' 
#' @importFrom stats cov mahalanobis ecdf
#' 
#' @noRd
feat2pval = function(x, nulldist, feat_idvals) {
	# null variance matrix
	sig = tryCatch(stats::cov(nulldist, use="complete.obs"), 
	error=function(u) {NULL})
	if(is.null(sig)) {
		return(NA)
	}
	if(any(is.na(sig))) {
		return(NA)
	}
	# null variance matrix inverse
	siginv = tryCatch(solve(sig), error=function(u) {NULL})
	if(is.null(siginv)) {
		return(NA)
	}
	# distance from ideal point
	reduced_obs = stats::mahalanobis(x, feat_idvals, siginv, inverted=TRUE)
	reduced_null = stats::mahalanobis(nulldist, feat_idvals, siginv, 
	inverted=TRUE)
	# p value
	pval = stats::ecdf(reduced_null)(reduced_obs)
	return(pval)
}
