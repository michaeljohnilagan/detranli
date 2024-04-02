#' Generate content-nonresponsive data
#' 
#' Generate a Likert-type sample where items are independent, provided the item response 
#' distributions.
#' 
#' @param n the sample size.
#' @param probs a list of probability vectors, one for each item.
#' 
#' @return The sample matrix.
#' 
#' @seealso [base::sample()]
#' 
#' @noRd
rcnr = function(n, probs) {
	dat = t(replicate(n, {
		sapply(probs, function(p) {
			sample(1:length(p), size=1, prob=p)
		})
	}))
	return(dat)
}
