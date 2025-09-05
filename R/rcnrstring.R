#' Generate content-nonresponsive data with a particular category having specified probability
#' 
#' Generate a Likert-type sample where items are independent and 
#' a single category has a specified (possibly high) probability of endorsement.
#' 
#' @inheritParams rcnrunif
#' @param prob Scalar or vector indicating probability of selecting desired
#'  categories specified in \code{whichcat}.
#' @param whichcat Scalar or vector indicating which category should have
#'  assigned probability.
#' @param allsame Boolean value indicating whether each participant should
#'  have the same response process.
#' 
#' @details
#' 
#' The goal of this function is to mimic the case where a participant is
#' nonresponsive and selects (mostly) the same category throughout the survey.
#' Multiple nonresponsive participants may have the exact same behavior, or may
#' differ.
#' 
#' If \code{whichcat} is \code{NULL}, a random category is chosen with uniform
#' probability for each subscale with the same number of categories. For
#' example, if \code{pointscales} contains a vector of 5's, a value inclusive
#' of 1 through 5 is chosen with equal probability.
#' 
#' If \code{whichcat} is a scalar value, this single category is used across
#' all items. This choice requires that this scalar value is compatible with
#' \code{pointscales}. It must be a category that could be observed for all
#' items.
#' 
#' If \code{whichcat} is a vector, then is should have the same length as
#' \code{pointscales} (i.e., the number of items) and each entry should be a
#' valid category for each item.
#' 
#' For each item, whatever category is chosen using the above scheme is assigned
#' probability equal to \code{prob}. The remaining categories have equal
#' probability out of whatever is left, 1-\code{prob}.
#' 
#' \code{allsame} toggles whether content nonresponsive participants all have
#' the same response process or not. If \code{TRUE}, all will have same process.
#' If \code{FALSE}, then \code{whichcat} should be specified as \code{NULL} so
#' that different participants have a different favorite category that is
#' randomly generated. Note that a wrapper or multiple calls with generating
#' subsets of participants from this function could effectively generate
#' something similar if more fine-grained control is desired.
#' 
#' Likert-type categories are from 1 (not 0) to K where K is the highest category.
#' 
#' @return The sample matrix.
#' 
#' @seealso \code{\link{sample}}, \code{\link{rcnrunif}}, \code{\link{rcnrbinom}},
#'  \code{\link{rcnrreverse}}, \code{\link{rcnrpat}}
#' 
#' @export
#' 
#' @examples 
#' set.seed(47)
#' 
#' # 50 participants, 10 5 category items, random fav cat, chosen at prob = .75
#' rcnrstring(50, rep(5, 10), prob=.75, whichcat=NULL)
#' 
#' # 50 participants, 10 5 category items, category 2 chosen at prob = .75
#' rcnrstring(50, rep(5, 10), prob=.75, whichcat=2)
#' 
#' # 50 participants, each with different preference at .80 prob
#' rcnrstring(50, rep(5, 10), prob=.8, whichcat=NULL, allsame=FALSE)
#' 
rcnrstring = function(n, pointscales, prob=.75, whichcat=NULL, allsame=TRUE) {
  
  if(length(prob)>1 | !is.numeric(prob)){
    stop("prob should be a single numeric value")
  }
  if (prob <=0 | prob > 1){
    stop("prob should be above 0 and below or equal to 1")
  }
  
  if(!allsame & !is.null(whichcat)){
    stop("allsame=FALSE currently only works if whichcat is left NULL")
  }
  
  if(allsame){

    # determine favorite category and probabilities    
    stringprobs = stringhelper(pointscales, whichcat, prob)
  
    # generate data
    dat = rcnr(n=n, probs=stringprobs$probs)
    #attr(dat, "probs") = stringprobs
        
  } else {
    
    # generate one at a time
    dat = t(sapply(1:n, function(i){
      stringprobs = stringhelper(pointscales, NULL, prob)
      rcnr(n=1, probs = stringprobs$probs)
    }))
   
  }
  
	return(dat)
}

stringhelper = function(pointscales, whichcat, prob){
  if(is.null(whichcat)){
    unique_pointscales = sort(unique(pointscales))
    favcat = sapply(unique_pointscales, function(k){sample(k, 1)})
    favcats = sapply(pointscales, function(k){favcat[unique_pointscales==k]})
  } else if (length(whichcat)==1){
    favcats = sapply(pointscales, function(k){
      if(!whichcat %in% 1:k){
        stop("whichcat is not a possible choice for some items' categories specified in pointscales")
      } else {
        whichcat
      }
    })
  } else if (length(whichcat)==length(pointscales)){
    favcats = sapply(1:length(pointscales), function(j){
      if(!whichcat[j] %in% 1:pointscales[j]){
        stop("whichcat is not a possible choice for some items' categories specified in pointscales")
      } else {
        whichcat[j]
      }
    })
  } else {
    stop("Something went wrong with specifying whichcat and/or pointscales")
  }
  
  probs = lapply(1:length(pointscales), function(j) {
    p = rep((1-prob)/(pointscales[j]-1), pointscales[j]) # prob vector
    p[favcats[j]] <- prob # replace favorite category
    p
  })
  
  return(list(favcats = favcats, probs=probs))
  
}
