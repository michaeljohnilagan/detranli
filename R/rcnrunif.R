#' Generate content-nonresponsive data with uniform distribution
#' 
#' Generate a Likert-type sample where items are independent and uniformly 
#' distributed, 
#' provided the number of response categories for each item.
#' Likert-type categories are from 1 (not 0) to K where K is the highest category.
#' 
#' @param n the sample size.
#' @param pointscales vector of integers indicating how many Likert-type response categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' 
#' @return The sample matrix.
#' 
#' @seealso \code{\link{sample}}, \code{\link{rcnrbinom}}, \code{\link{rcnrpat}},
#'  \code{\link{rcnrreverse}}, \code{\link{rcnrstring}}
#' 
#' @export
#' 
#' @examples
#' set.seed(47)
#' rcnrunif(30, c(4, 4, 4, 5, 5, 5, 6, 6, 6)) # 30 participants, 9 items
rcnrunif = function(n, pointscales) {
	# create probability vectors
	probs = lapply(pointscales, function(k) {
			rep(1/k, times=k)
	})
	# generate
	dat = rcnr(n=n, probs=probs)
	return(dat)
}
