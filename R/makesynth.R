#' Make synthetic data
#' 
#' Generate synthetic data for the i-th null hypothesis: that the i-th response vector 
#' is exchangeable.
#' 
#' This function accommodates multiple point-scales. 
#' Items are grouped by point-scale, and responses are permuted only within each group.
#' 
#' @param i the row index of the respondent of interest.
#' @param data the Likert-type data (matrix or dataframe), full sample.
#' @param pointscales vector of integers indicating how many Likert-type response categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' @param numperms an integer indicating how many permutations to generate.
#' 
#' @return The empirical null distribution, a matrix where the first row is the observed response vector.
#' 
#' @noRd
makesynth = function(i, data, pointscales, numperms=200) {
	# prepare to do shuffling within pointscales
	obs = unname(as.matrix(data)[i,])
	# column index vs pointscale
	originalposition = (1:length(pointscales))[order(pointscales)]
	# shuffle within pointscale
	unique_pointscales = sort(unique(pointscales))
	bypointscale = lapply(unique_pointscales, function(k) {
		obs_thispointscale = obs[which(pointscales==k)]
		do.call(rbind, replicate(numperms, {
			sample(obs_thispointscale)
		}, simplify=FALSE))
	})
	synth_disordered = do.call(cbind, bypointscale)
	# get into correct order
	synth = synth_disordered[, order(originalposition)]
	# put together
	together = rbind(obs, synth)
	return(together)
}
