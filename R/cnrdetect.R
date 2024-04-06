#' Full content-nonresponsivity detection
#' 
#' Compute p-values for each respondent.
#' If the number of response options is the same for all items, 
#' the null hypothesis is that the response vector is exchangeable.
#' If the number of response options is not all the same, 
#' the null hypothesis is that: 
#' each subset of items that have the same number of response options is exchangeable; 
#' and subsets of items having different numbers of response options are independent.
#' 
#' Likert-type response data must be 1 to K where K is the highest response category. 
#' Missing values must be \code{NA}.
#' 
#' For the row tested, missing items are excluded from the test.
#' When no more than one item is nonmissing, p-value defaults to \code{NA}.
#' For the anchor sample, cells that have not been excluded but are missing are 
#' imputed with the midrange (e.g. "3" on a scale of 1 to 5).
#' 
#' When there are issues computing feature-space covariances, 
#' p-value defaults to \code{NA}.
#' 
#' @param data Likert-type data (matrix or dataframe).
#' @param pointscales vector of integers indicating how many Likert-type response 
#' categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#' @param feat_funs list of nonresponsivity feature functions. Each function must 
#' return a numeric vector.
#' @param feat_idvals vector of ideal values, in the same order as \code{feat_funs}.
#' @param numperms an integer indicating how many permutations to produce per respondent.
#' 
#' @return Vector of p-values. Smaller p-values are less suspicious.
#' 
#' @export
cnrdetect = function(data, pointscales, numperms=1e3,
feat_funs=c(mahal, ptcossim), feat_idvals=c(0, +1)) {
	# assert
	stopifnot(ncol(data)==length(pointscales))
	stopifnot(length(feat_funs)==length(feat_idvals))
	# combine features into one function
	feats_combo = function(x, ref) {
		sapply(feat_funs, function(f) {
			f(x, ref)
		})
	}
	# missing handling method
	missingmethod = "pointscalemidrange"
	# calculate hypothesis tests
	pval = sapply(1:nrow(data), function(i) {
		# consider only nonmissing
		idx_nonmiss = which(!is.na(data[i,]))
		if(length(idx_nonmiss)<=1) {
			return(NA)
		} # emergency exit: no more than one item nonmissing
		# likert space
		obs_likert = unname(data[i,idx_nonmiss])
		ref = as.matrix(data[-i,idx_nonmiss,drop=FALSE])
		if(missingmethod=="pointscalemidrange") {
			ref_completed = imputepsm(ref, pointscales[idx_nonmiss])
		} # fill in missing in anchor set
		synth_likert = makesynth(i, data[,idx_nonmiss,drop=FALSE], 
		pointscales=pointscales[idx_nonmiss], numperms=numperms)
		# feature space
		obs_nri = feats_combo(rbind(obs_likert), ref_completed)
		synth_nri = feats_combo(synth_likert, ref_completed)
		# p values
		feat2pval(obs_nri, synth_nri, feat_idvals=feat_idvals)
	})
	return(pval)
}
