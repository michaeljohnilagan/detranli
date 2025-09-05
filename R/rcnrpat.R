#' Generate content-nonresponsive data using a pattern of response
#' 
#' Generate a Likert-type sample to mimic the case where a participant may use
#' some nonresponsive pattern to complete the items.
#' 
#' @inheritParams rcnrunif
#' @param type Pattern type. "updown", "endpoints", "step". See details.
#' @param prob Probability that pattern will be broken for any given item.
#' @param nstep If "step", how many times a response is typically repeated.
#' @details
#' 
#' The goal of this function is to mimic the case where a participant is
#' nonresponsive and completes the survey using one of several patterns.
#' 
#' With "updown", participants complete items in a zig-zag pattern. e.g.,
#' 3, 4, 5, 4, 3, 2, 1, 2, 3, 4, etc. A random category is chosen as a starting
#' point and if the survey switches between subscales with different numbers of
#' categories per item (as specified by \code{pointscales}), the pattern also
#' starts over again at a random location. Item responses are not independent in
#' this case as the response to one item is often dependent on the participant's
#' response to a previous item.
#' 
#' With "endpoints", responses randomly toggle between the lowest and highest
#' category.
#' 
#' With "step", responses are similar to "updown" but the same response may be
#' repeated multiple times. e.g., 111222333.
#' 
#' If \code{prob} is non-zero, \code{prob} of the time some other category is
#' chosen with uniform probability.
#' 
#' Likert-type categories are from 1 (not 0) to K where K is the highest category.
#' 
#' @return The sample matrix.
#' 
#' @seealso \code{\link{sample}}, \code{\link{rcnrunif}}, \code{\link{rcnrbinom}},
#'  \code{\link{rcnrreverse}}, \code{\link{rcnrstring}}
#' 
#' @export
#' 
#' @examples 
#' set.seed(47)
#' 
#' # 20 participants, 10 5 category items, updown or zig-zag pattern
#' rcnrpat(20, rep(5, 10), type="updown")
#' 
#' # 20 participants, 10 5 category items, mostly endpoints
#' rcnrpat(20, rep(5, 10), type="endpoints")
#' 
#' # 20 participants, 10 5 category items, step pattern
#' rcnrpat(20, rep(5, 10), type="step", nstep=4)
#' 
#' 
rcnrpat = function(n, pointscales, type=c("updown","endpoints","step"),
                   prob=.05, nstep=4) {
  
  type = match.arg(type)
  
  if(length(prob)>1 | !is.numeric(prob)){
    stop("prob should be a single numeric value")
  }
  if (prob < 0 | prob >= 1){
    stop("prob should be equal to or above 0 and below 1")
  }
  if(nstep < 1){
    stop("nstep should be an integer, 1 or greater")
  }
  
  if(type=="updown"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      inc = 1
      for(j in 2:length(pointscales)){

        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else if (pointscales[j] != pointscales[j-1]) { # change in pointscales?
          tmp = c(tmp, sample(pointscales[j], 1))
        } else { # continue pattern
          if(tmp[j-1] == pointscales[j]){ # reached top of scale
            inc = -1            
          } else if (tmp[j-1] == 1) { # reached bottom of scale
            inc = 1
          } 
          tmp = c(tmp, tmp[j-1]+inc)
        }
      }
      tmp
    }))
    
  } else if (type=="endpoints"){
    dat = t(sapply(1:n, function(i){
      tmp = numeric()
      for(j in 1:length(pointscales)){
        
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else {
          tmp = c(tmp, sample(c(1,pointscales[j]), 1))
        }
      }
      tmp
    }))
  } else if (type=="step"){
    dat = t(sapply(1:n, function(i){
      tmp = sample(pointscales[1], 1)
      inc = 1
      nrep = 1
      for(j in 2:length(pointscales)){
        
        # get random response?
        if(sample(c(TRUE,FALSE), 1, prob = c(prob, 1-prob))){
          tmp = c(tmp, sample(pointscales[j], 1))
        } else if (pointscales[j] != pointscales[j-1]) { # change in pointscales?
          tmp = c(tmp, sample(pointscales[j], 1))
        } else { # continue pattern
          if(nrep < nstep){
            tmp = c(tmp, tmp[j-1]) # same response as last time
            nrep = nrep + 1
          } else {
            if(tmp[j-1] == pointscales[j]){ # reached top of scale
              inc = -1
              nrep = 1
            } else if (tmp[j-1] == 1) { # reached bottom of scale
              inc = 1
              nrep = 1
            }
            tmp = c(tmp, tmp[j-1]+inc)
            nrep = 1
          }
        }
      }
      tmp
    }))
  }
  
	return(dat)
}
