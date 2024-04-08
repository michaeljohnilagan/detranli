#' Classification performance metrics
#' 
#' Given true class labels and predicted class labels, compute metrics like accuracy.
#' 
#' @param y vector of true class labels. 
#' Can be numeric (0 vs 1) or character ("nonrandom vs random") format.
#' @param yhat vector of predicted class labels. 
#' Can be numeric (0 vs 1) or character ("spare" vs "flag") format.
#' 
#' @return List of two components. 
#' First component is a confusion matrix, 
#' and second component is a vector of accuracy, specificity, sensitivity, 
#' positive predictive value, negative predictive value, 
#' and flag rate.
#' 
#' @export
#'
#' @examples
#' set.seed(47)
#' y = rep(0:1, times=6) # true class labels
#' yhat = rep(0:1, each=6) # predicted class labels
#' metrics(y, yhat) # get confusion matrix, accuracy, etc
metrics = function(y, yhat) {
	# assert
	stopifnot(all(y%in%(0:1))|all(y%in%c("nonrandom", "random")))
	stopifnot(all(yhat%in%(0:1))|all(yhat%in%c("flag", "spare"))|
	all(yhat%in%c("nonrandom", "random")))
	# confusion table
	confusion = table(y, yhat)
	# class label format
	if(!is.numeric(y)) {
		y = ifelse(y=="random", 1, 0)
	}
	if(!is.numeric(yhat)) {
		yhat = ifelse(yhat=="flag", 1, 0)
	}
	# compute metrics
	acc = mean(y==yhat) # accuracy
	spec = mean(yhat[y==0]==0) # specificity
	sens = mean(yhat[y==1]==1) # sensitivity
	ppv = mean(y[yhat==1]==1) # positive predictive value
	npv = mean(y[yhat==0]==0) # negative predictive value
	flagrate = mean(yhat==1) # flag rate
	# put together
	outcomemeasures = list(acc=acc, spec=spec, sens=sens, 
	ppv=ppv, npv=npv, flagrate=flagrate)
	return(list(confusion=confusion,
	outcomemeasures=unlist(outcomemeasures)))
}
