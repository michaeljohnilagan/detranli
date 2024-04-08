#' Person-total cosine similarity
#' 
#' Calculates the person-total cosine similarity with respect to some reference sample.
#' 
#' @inheritParams mahal
#' 
#' @return Vector of person-total cosine similarities.
#' 
#' @export
#'
#' @examples
#' x = cnrexample1[1:10,]
#' anchor = cnrexample1[-(1:10),]
#' ptcossim(x, anchor) # cosine similarity of x wrt to anchor
ptcossim = function(x, ref) {
	# get anchor parameters
	mu = colMeans(ref)
	# compute statistic
	ptc = apply(x, 1, function(v) {
		nume = sum(v*mu)
		deno = sqrt(sum(v*v)*sum(mu*mu))
		nume/deno
	})
	return(ptc)
}
