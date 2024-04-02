#' Generate content-nonresponsive data with binomial distribution
#' 
#' Generate a Likert-type sample where items are independent and 
#' follow a fair-coin binomial distribution, 
#' provided the number of response categories for each item.
#' Likert-type categories are from 1 (not 0) to K where K is the highest category.
#' 
#' @inheritParams rcnrunif
#' 
#' @return The sample matrix.
#' 
#' @seealso [stats::dbinom()], [base::sample()] , [rncr()]
#' 
#' @importFrom stats dbinom
#' 
#' @export
#' 
#' @examples 
#' set.seed(47)
#' rcnrbinom(30, c(4, 4, 4, 5, 5, 5, 6, 6, 6)) # 30 participants, 9 items
rcnrbinom = function(n, pointscales) {
	# create probability vectors
	probs = lapply(pointscales, function(k) {
			stats::dbinom(0:(k-1), size=k-1, prob=0.5)
	})
	# generate
	dat = rcnr(n=n, probs=probs)
	return(dat)
}
