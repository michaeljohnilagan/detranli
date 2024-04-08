#' Person-total correlation
#' 
#' Calculates the person-total correlation with respect to some reference sample.
#' 
#' @inheritParams mahal
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
ptcor = function(x, ref) {
	# get anchor parameters
	mu = colMeans(ref)
	# compute statistic
	ptc = apply(x, 1, function(v) {
		cor(v, mu)
	})
	ptc = ifelse(is.na(ptc), -1, ptc) # impute over undefined
	return(ptc)
}
