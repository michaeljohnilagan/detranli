#' Mahalanobis distance
#' 
#' Calculates the Mahalanobis distance with respect to some reference sample.
#' 
#' @param x data (matrix or dataframe) of interest.
#' @param ref reference sample (matrix or dataframe). Must have same number of columns as \code{x}.
#' @param mu (Optional) Vector of means that could be used instead of the reference sample.
#' @param sig (Optional) Covariance matrix that could be used instead of the reference sample.
#' @param missargs (Experimental/Optional) A list of arguments for handling missing data. Currently
#'  a list of two elements \code{missingmethod} and \code{idx_nonmiss} are accepted. Details will
#'  later document how these work. This part is likely to change and only handles missing
#'  values in the reference sample.
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
#' @importFrom EMgaussian em.cov
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' anchor = cnrexample1[-(1:10),]
#' mahal(x, anchor) # mahalanobis distance of x wrt to anchor
mahal = function(x, ref, mu=NULL, sig=NULL, missargs=NULL) {
  
  # missing data handling
  if((is.null(mu)|is.null(sig))&!is.null(missargs)){
    if(missargs$missingmethod=="EM"){
      anchors = em.cov(ref, max.iter=1000, tol=1e-04, start="diag")
      if(!anchors$conv){warning("EM algorithm for mahal missing data may not have converged")}
    } else if (missargs$missingmethod=="pointscalemidrange") {
      # do nothing
    } else {
      stop("Unknown missing data handling method")
    }
  }
	# get anchor parameters
  if(is.null(mu)){
    if(is.null(missargs)){
      mu = colMeans(ref)
    } else if (missargs$missingmethod=="pointscalemidrange") {
      mu = colMeans(ref)
    } else if (missargs$missingmethod=="EM"){
      mu = anchors$mu[missargs$idx_nonmiss]      
    }
  }
  if(is.null(sig)){
    if(is.null(missargs)){
      sig = cov(ref)
    } else if (missargs$missingmethod=="pointscalemidrange") {
      sig = cov(ref)      
    } else if (missargs$missingmethod=="EM"){
      sig = anchors$S[missargs$idx_nonmiss,missargs$idx_nonmiss]
    }
  }

	# compute statistic
  sqmahal = stats::mahalanobis(x, center=mu, cov=sig)    

	mahal = sqrt(sqmahal) # take square root
	return(unname(mahal))
}