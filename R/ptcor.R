#' Person-total correlation
#' 
#' Calculates the person-total correlation with respect to some reference sample.
#' 
#' @inheritParams mahal
#' 
#' @details
#' Computes person-total correlation for each row in \code{x} based on either the
#' references sample in \code{ref} or the means in \code{mu}. Lower values are
#' typically thought to be indicative of careless responders. A perfect +1
#' value indicates the ideal point.
#' 
#' @return Vector of person-total correlations. Undefined values default to -1.
#' 
#' @seealso \code{\link[stats]{cor}}
#' 
#' @importFrom stats cor
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' anchor = cnrexample1[-(1:10),]
#' ptcor(x, anchor) # person total correlation of x wrt to anchor
ptcor = function(x, ref, mu=NULL) {
	# get anchor parameters
  if(is.null(mu)){mu = colMeans(ref)}
  # compute statistic
	ptc = apply(x, 1, function(v) {
		cor(v, mu)
	})
	ptc = ifelse(is.na(ptc), -1, ptc) # impute over undefined
	return(ptc)
}
