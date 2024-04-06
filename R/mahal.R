#' Mahalanobis distance
#' 
#' Calculates the Mahalanobis distance with respect to some reference sample.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' 
#' @return Vector of Mahalanobis distances.
#' 
#' @seealso \code{\link[stats]{mahalanobis}}
#' 
#' @importFrom stats mahalanobis cov
#' 
#' @export
mahal = function(x, ref) {
	# get anchor parameters
	mu = colMeans(ref)
	sig = cov(ref)
	# compute statistic
	sqmahal = stats::mahalanobis(x, center=mu, cov=sig)
	mahal = sqrt(sqmahal) # take square root
	return(unname(mahal))
}