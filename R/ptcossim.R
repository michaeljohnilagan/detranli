#' Person-total cosine similarity
#' 
#' Calculates the person-total cosine similarity with respect to some reference sample.
#' 
#' @inheritParams mahal
#' 
#' @details
#' Computes person-total cosine similarity for each row in \code{x} based on either the
#' references sample in \code{ref} or the means in \code{mu}. Lower values are
#' typically thought to be indicative of careless responders. A perfect +1
#' value indicates the ideal point. Although the ideal point is the same as
#' person-total correlations, the metric for this index is not the same.
#' 
#' @return Vector of person-total cosine similarities.
#' 
#' @importFrom EMgaussian em.cov 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' anchor = cnrexample1[-(1:10),]
#' ptcossim(x, anchor) # cosine similarity of x wrt to anchor
ptcossim = function(x, ref, mu=NULL, missargs=NULL) {
  
  # missing data handling
  if(is.null(mu)&!is.null(missargs)){
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

	# compute statistic
	ptc = apply(x, 1, function(v) {
		nume = sum(v*mu)
		deno = sqrt(sum(v*v)*sum(mu*mu))
		nume/deno
	})
	return(ptc)
}
