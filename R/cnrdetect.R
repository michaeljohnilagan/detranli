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
#' @param feat_funs vector of nonresponsivity feature function names. Each function must 
#' return a numeric vector. Defaults to Mahalanobis distance and person-total
#' cosine similarity. See Details.
#' @param feat_idvals vector of ideal values, in the same order as \code{feat_funs}.
#' Defaults to 0 and +1 as the ideal points for Mahalanobis distance and person-total
#' cosine similarity.
#' @param numperms an integer indicating how many permutations to produce per respondent.
#' @param details If \code{FALSE} (the default), returns only p-values. If
#' \code{TRUE}, returns additional information in a list.
#' @param custom_funs (Optional) named list containing name-function pairs for
#' user-defined feature functions. See Details.
#' 
#' @details
#' Performs the permutation test in Ilagan and Falk (2023) for detecting survey
#' bots (or random responders). Assuming items are exchangeable for bots (but not
#' for diligent humans), p-values represent a test of the null hypothesis that
#' each row is a bot. Synthetic bots are generated for each row by permuting the
#' responses in that row. The selected nonresponsitivity indices (\code{feat_funs})
#' for each row and its synthetic bots are computed using leave-one-out calculations
#' with the remainder of the observed data as a reference sample; the distance of
#' these indices from their ideal points is computed and collapsed into a
#' one-dimensional space using a distance measure (itself resembling Mahalanobis
#' distance). p-values are then the lower probability of the observed
#' response versus those of its synthetic bots in this one-dimensional space.
#' 
#' Note that \code{feat_funs} will look at the names in \code{custom_funs}, if
#' defined, for nonresponsitivity index functions prior to looking in the package
#' namespace.
#'   
#' Custom-defined feature functions (or nonresponsitivity indices) are possible
#' by passing a named list to \code{custom_funs}.  Function signatures for each
#' function should be \code{function(x, ref)} where \code{x} is a vector or data
#' frame of observations whose features we seek and \code{ref} is a data frame
#' to use for the reference sample, if required. For instance, \code{ref} would
#' contain a reference sample used to compute column means of some items with those
#' means later used in the calculation of person-total correlation.
#' 
#' @return Output depends on the value of \code{details}. If \code{FALSE}, a
#' vector of p-values. Smaller p-values are less suspicious. If not \code{TRUE},
#' a list with the following elements:
#' 
#' \itemize{
#'  \item{\code{pvals}: a vector of p-values as already described above.}
#'  \item{\code{obs_nris}: a matrix of nonresponsivity (feature) indices for each
#'    row of the dataset.}
#'  \item{\code{synth_nris}: a list where each element contains a matrix of
#'    nonresponsivity (feature) indices for the synthetic bots for each row.
#'    The length of this list is equal to the number of rows in the dataset.
#'    Each matrix therein has the same number of rows as permutations, and number
#'    of columns as features.}
#'  \item{\code{synth_likert}: a list where each element contains a matrix of
#'    permuted Likert-type vectors representing the synthetic bots for each row.
#'    That is, the permutations of the original row in the dataset.}
#' }
#' 
#' @export
#'
#' @examples
#' set.seed(47)
#' 
#' # p values
#' pvals = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#' numperms=1000) 
#' 
#' # If wishing to do classification with, 95% sensitivity
#' flags = ifelse(pvals < .05, "human", "bot")
#' 
#' # Obtain more than just p-values
#' permresults = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#' numperms=1000, details=TRUE)
#' 
#' # again, generate flags
#' flags = ifelse(permresults$pvals < .05, "human", "bot")
#' 
#' # Take a look at the indices of a probable human versus its synthetic bots
#' # Note that the ease of extracting such output is a work in progress.
#' # Also note that in plots, the ideal point (least suspicious) is (0,+1), which
#' # is in the direction of the upper-left corner of each plot.
#' idx = which(flags=="human")[1]
#' 
#' # plot its synthetic bots
#' plot(permresults$synth_nris[[idx]], xlab="MD", ylab="ptcossim",
#'      xlim=c(4,6.5), ylim=c(.75,.9))
#' 
#' # add point for this probable human:
#' points(x=permresults$obs_nris[idx,1], y=permresults$obs_nris[idx,2], col="blue",
#'   pch=19)
#' 
#' # Now look at a probably bot versus its synthetic bots
#' idx = which(flags=="bot")
#' idx = idx[length(idx)]
#' 
#' # plot its synthetic bots
#' plot(permresults$synth_nris[[idx]], xlab="MD", ylab="ptcossim",
#'      xlim=c(3.5,5.5), ylim=c(.85,.95))
#' 
#' # add point for this probable human:
#' points(x=permresults$obs_nris[idx,1], y=permresults$obs_nris[idx,2], col="red",
#'   pch=19)
#' 
#' 
cnrdetect = function(data, pointscales, numperms=1e3,
feat_funs=c("mahal", "ptcossim"), feat_idvals=c(0, +1),
details=FALSE, custom_funs=NULL) {
  
	# assert
	stopifnot(ncol(data)==length(pointscales))
	stopifnot(length(feat_funs)==length(feat_idvals))
	if(!is.null(custom_funs)){stopifnot(is.list(custom_funs))}
	
	# combine features into one function
	feats_combo = function(x, ref) {
		sapply(feat_funs, function(f) {
		  if(!is.null(custom_funs)){
		    if(f %in% names(custom_funs)){
		      f <- custom_funs[[f]]
		    } else {
		      f <- get(f)
		    }
		  } else {
		    f <- get(f)
		  }
			f(x, ref)
		})
	}
	# missing handling method
	missingmethod = "pointscalemidrange"
	# calculate hypothesis tests
	permlist = lapply(1:nrow(data), function(i) {
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
		pval <- feat2pval(obs_nri, synth_nri, feat_idvals=feat_idvals)
		
		return(list(pval=pval, obs_nri=setNames(obs_nri, feat_funs), synth_nri=synth_nri[-1,],
		            synth_likert=synth_likert[-1,]))
		
	})

	pvals <- sapply(permlist, "[[", i="pval")
	
	# For backward compatibility
	# Otherwise could default to returning the entire list
	if(details){
	  obs_nris <- t(sapply(permlist, "[[", i="obs_nri"))
	  synth_nris <- lapply(permlist, "[[", i="synth_nri")
	  synth_likert <- lapply(permlist, "[[", i="synth_likert")
	  return(list(pvals=pvals, obs_nris=obs_nris, synth_nris=synth_nris,
	              synth_likert=synth_likert))
	} else {
	  return(pvals)
	}
	
}
