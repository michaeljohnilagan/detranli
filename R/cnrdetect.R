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
#' @param missingmethod Method for handling missing data. See Details. Options are \code{"pointscalemidrange"} or \code{"EM"}. 
#' @param type Create synthetic data by doing permutations (\code{"perm"}),
#' from a first-order Markov Chain (\code{"markovchain"}) with the same
#' transition probabilities as the row, or from a higher-order markov
#' chain (\code{"homarkovchain"}), or by considering possible n-grams of
#' response combinations (\code{"ngram"}). Defaults to permutations.
#' @param mcorder Positive integer that refers to the order of the markov chain or
#' "n" for the n-gram approach.
#' @param highestprob When \code{"homarkovchain"} is chosen, whether to simulate
#' using the next category that has the highest probability (\code{TRUE}) or
#' sample based on the available probabilities (\code{FALSE}). 
#' @param parallel_type (Experimental) If desired, the method of parallel processing. Each row's test
#' can be executed on a separate processing core. Options are \code{"lapply"} (default, no parallelization),
#' \code{"parallel"} (uses parallel package), \code{"furrr"} (uses furrr package). The latter two may
#' speed up computations considerably provided the computer has more than a couple processing cores to use.
#' @param ncores (Experimental/Optional) Number of processing cores to use if doing parallel processing.
#' @param seed (Optional) A random number seed, if you want the function to set the seed. Defaults to \code{NULL}.
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
#' by passing a named list to \code{custom_funs}. Function signatures for each
#' function should be \code{function(x, ref, ..., missargs)} where \code{x} is a
#' vector or data frame of observations whose features we seek, \code{ref} is a
#' data frame to use for the reference sample, if required, \code{...} is not
#' used internally but the custom function may have other options (e.g., see
#' \code{\link{mahal}}), and \code{missargs} accepts a list of possible missing
#' data handling options for the reference sample (see later). For instance,
#' \code{ref} would contain a reference sample used to compute column means of
#' some items with those means later used in the calculation of person-total
#' correlation.
#' 
#' For handling missing data, only observations that each row actually completes
#' will be used in their permutation test. However, other rows in the reference
#' sample may have missing data, which complicates computation of means and 
#' covariances for feature functions such as Mahalanobis distance, person-total
#' correlation, and person-total cosine similarity. The default currently uses
#' \code{"pointscalemidrange"}, which is essentially middle value imputation.
#' Not great, but what Dupuis et al (2018) used and found to do well enough in
#' establishing utility of such indices. \code{"EM"} is experimental and uses
#' the EM algorithm to obtain a saturated mean and covariance matrix, assuming
#' multivariate normal data from the reference sample. This option is passed 
#' down to feature functions, which have been written to implement this option.
#' Custom defined feature functions will not implement this approach unless told
#' to do so, though the exact way to do this is not yet documented. See
#' \code{\link{mahal}} for an example. Either way the test may not work well for
#' rows with high rates of missing data.
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
#' @importFrom parallel makeCluster clusterSetRNGStream parLapply stopCluster
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan multicore multisession sequential
#' @importFrom parallelly supportsMulticore
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' set.seed(47)
#' 
#' # p values
#' pvals = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#'   numperms=1000) 
#' 
#' # If wishing to do classification with, 95% sensitivity
#' flags = ifelse(pvals < .05, "human", "bot")
#' 
#' # Obtain more than just p-values
#' permresults = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#'   numperms=1000, details=TRUE)
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
#' # add point for this probable bot:
#' points(x=permresults$obs_nris[idx,1], y=permresults$obs_nris[idx,2], col="red",
#'   pch=19)
#' 
#' 
#' # Parallel processing examples
#' 
#' # Using parallel package, 2 cores:
#' pvals = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#'    numperms=1000, parallel_type="parallel", ncores = 2, seed=1234)
#' 
#' # Using furrr package, 2 cores:
#' pvals = cnrdetect(cnrexample1, pointscales=rep(5, times=ncol(cnrexample1)), 
#'    numperms=1000, parallel_type="furrr", ncores = 2, seed=1234)
cnrdetect = function(data, pointscales, numperms=1e3,
feat_funs=c("mahal", "ptcossim"), feat_idvals=c(0, +1),
details=FALSE, custom_funs=NULL, missingmethod=c("pointscalemidrange","EM"),
synthtype=c("perm","markovchain","homarkovchain"), mcorder=2, highestprob=FALSE,
parallel_type=c("lapply","parallel","furrr"), ncores=NULL, seed=NULL) {
  
	# check input
	stopifnot(ncol(data)==length(pointscales))
	stopifnot(length(feat_funs)==length(feat_idvals))
	if(!is.null(custom_funs)){stopifnot(is.list(custom_funs))}
	parallel_type=match.arg(parallel_type)
	missingmethod=match.arg(missingmethod)
	
	# Check data vs pointscales
	# Currently assumes coding 1,2,... with NAs allowed
	pointcheck = all(sapply(1:ncol(data), function(j){ (data[,j] %in% 1:pointscales[j])|is.na(data[,j])}))
	if(!pointcheck){stop("cnrdetect assumes items are coded as integers starting from 1 (e.g., 1 2 3 4 5 for 5 categories).
	                     In checking the data versus pointscales, a mismatch was found.")}
	
	# combine features into one function
	feats_combo = function(x, ref, missargs) {
		sapply(feat_funs, function(f) {
		  if(!is.null(custom_funs)){
		    if(f %in% names(custom_funs)){
		      f = custom_funs[[f]]
		    } else {
		      f = get(f)
		    }
		  } else {
		    f = get(f)
		  }
			f(x, ref, missargs=missargs)
		})
	}

	# function to do everything for a single row
	permfun = function(i, data, pointscales, numperms, feats_combo, missingmethod){
	  
	  # consider only nonmissing for a given row
	  idx_nonmiss = which(!is.na(data[i,]))
	  if(length(idx_nonmiss)<=1) {
	    return(list(pval=NA, obs_nri=setNames(rep(NA, length(feat_funs)), feat_funs), synth_nri=NA,
	                synth_likert=NA))
	  } # emergency exit: no more than one item nonmissing
	  # likert space
	  obs_likert = unname(data[i,idx_nonmiss])

	  # synthetic bots
	  synth_likert = makesynth(i, data[,idx_nonmiss,drop=FALSE],
	                           pointscales=pointscales[idx_nonmiss], numperms=numperms,
	                           type=synthtype, mcorder=mcorder, highestprob=highestprob)
	  
	  # missing data handling
	  # fill in missing in anchor set
	  if(missingmethod=="pointscalemidrange") {
	    ref = as.matrix(data[-i,idx_nonmiss,drop=FALSE])
	    ref_completed = imputepsm(ref, pointscales[idx_nonmiss])
	  } else {
	    ref_completed = as.matrix(data[-i, , drop=FALSE]) # don't fill in
	  }
	  
	  # feature space
	  obs_nri = feats_combo(rbind(obs_likert), ref_completed, missargs=list(missingmethod=missingmethod, idx_nonmiss=idx_nonmiss))
	  synth_nri = feats_combo(synth_likert, ref_completed, missargs=list(missingmethod=missingmethod, idx_nonmiss=idx_nonmiss))
	  
	  # p values
	  pval = feat2pval(obs_nri, synth_nri, feat_idvals=feat_idvals)
	  
	  return(list(pval=pval, obs_nri=setNames(obs_nri, feat_funs), synth_nri=synth_nri[-1,],
	              synth_likert=synth_likert[-1,]))
	}
	
	# calculate hypothesis tests
	if(parallel_type=="lapply"){
	  if(!is.null(seed)){
	    set.seed(seed)
	  }
	  permlist = lapply(1:nrow(data), permfun, data=data, pointscales=pointscales,
	                    numperms=numperms, feats_combo=feats_combo, missingmethod=missingmethod)
	} else if (parallel_type=="parallel") {
	  cl = makeCluster(ncores)
	  clusterSetRNGStream(cl, seed)
	  permlist = parLapply(cl, as.list(1:nrow(data)), permfun, data=data, pointscales=pointscales,
	                 numperms=numperms, feats_combo=feats_combo, missingmethod=missingmethod)
	  stopCluster(cl)
	} else if (parallel_type=="furrr"){
	  if(supportsMulticore()){
	    plan(multicore, workers=ncores)
	  } else {
	    plan(multisession, workers=ncores)
	  }
	  permlist = future_map(1:nrow(data),  permfun, data=data, pointscales=pointscales,
	                  numperms=numperms, feats_combo=feats_combo, missingmethod=missingmethod,
	                  .options = furrr_options(seed=seed),
	                  .progress=TRUE)
	  
	  #FIXME check giving me errors unless I do this
	  if(!supportsMulticore()){
	    plan(sequential)
	  }
	}	


	pvals = sapply(permlist, "[[", i="pval")

	# For backward compatibility
	# Otherwise could default to returning the entire list
	if(details){
	  obs_nris = t(sapply(permlist, "[[", i="obs_nri"))
	  synth_nris = lapply(permlist, "[[", i="synth_nri")
	  synth_likert = lapply(permlist, "[[", i="synth_likert")
	  return(list(pvals=pvals, obs_nris=obs_nris, synth_nris=synth_nris,
	              synth_likert=synth_likert))
	} else {
	  return(pvals)
	}
	
}
