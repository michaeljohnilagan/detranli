#' Mahalanobis distance
#' 
#' Calculates the Mahalanobis distance with respect to some reference sample.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' @param mu (Optional) Vector of means that could be used instead of the reference sample.
#' @param sig (Optional) Covariance matrix that could be used instead of the reference sample.
#' 
#' @details
#' Computes Mahalanobis distance for each row in \code{x} based on either the
#' references sample in \code{ref} or the means and covariance matrix in \code{mu}
#' and \code{sig}. Larger values of Mahalanobis distance are often thought to be
#' indicative of multivariate outliers or careless responders. Values at the
#' centroid of the means/covariances have a value of 0 (the ideal point).
#' 
#' @return Vector of Mahalanobis distances.
#' 
#' @seealso \code{\link[stats]{mahalanobis}}
#' 
#' @importFrom stats mahalanobis cov
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' anchor = cnrexample1[-(1:10),]
#' mahal(x, anchor) # mahalanobis distance of x wrt to anchor
mahal = function(x, ref, mu=NULL, sig=NULL) {
	# get anchor parameters
	if(is.null(mu)){mu = colMeans(ref)}
	if(is.null(sig)){sig = cov(ref)}
	# compute statistic
	sqmahal = stats::mahalanobis(x, center=mu, cov=sig)
	mahal = sqrt(sqmahal) # take square root
	return(unname(mahal))
}