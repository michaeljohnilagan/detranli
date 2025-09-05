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
#' 
#' @return The empirical null distribution, a matrix where the first row is the observed response vector.
#' 
#' @import markovchain
#' @import ngram
#' @noRd
makesynth = function(i, data, pointscales, numperms=200,
                     type=c("perm","markovchain","homarkovchain","ngram"),
                     mcorder = 2, highestprob = FALSE) {
  
  type = match.arg(type)
  
  # prepare to do shuffling within pointscales
  obs = unname(as.matrix(data)[i,])
  # column index vs pointscale
  originalposition = (1:length(pointscales))[order(pointscales)]
  # shuffle within pointscale
  unique_pointscales = sort(unique(pointscales))
  

  if(type=="perm"){
    bypointscale = lapply(unique_pointscales, function(k) {
      obs_thispointscale = obs[which(pointscales==k)]
      do.call(rbind, replicate(numperms, {
        sample(obs_thispointscale)
      }, simplify=FALSE))
    })    
  } else if (type=="markovchain"){
    bypointscale = lapply(unique_pointscales, function(k) {
      
      obs_thispointscale = obs[which(pointscales == k)]
      
      mcfit = markovchainFit(factor(obs[which(pointscales == k)], levels=1:k),
                              sanitize=TRUE)
      mc = new("markovchain", states = colnames(mcfit$estimate@transitionMatrix),
                    transitionMatrix = mcfit$estimate@transitionMatrix)
      # simulate from chain
      do.call(rbind, replicate(numperms, {
        as.numeric(markovchainSequence(length(obs_thispointscale), mc))
      }, simplify = FALSE))
    })
  } else if (type=="homarkovchain"){
    bypointscale = lapply(unique_pointscales, function(k) {
      
      obs_thispointscale = obs[which(pointscales == k)]
      if(mcorder < 1){stop("mcorder should be an integer, 1 or greater")}
      mcorder = floor(mcorder)
      
      mcfit = try(fitHigherOrder(obs_thispointscale, mcorder))
      while(inherits(mcfit, "try-error")){
        mcorder = mcorder - 1
        if(mcorder < 1){
          # TODO: more elegant way to fail
          stop("simulating from markov chain failed for at least one row in the dataset")
        }
        mcfit = try(fitHigherOrder(obs_thispointscale, mcorder))
      }
      
      # simulate from chain
      do.call(rbind, replicate(numperms, {
        as.numeric(simHigherOrder(mcfit, length(obs_thispointscale),
                                  highestprob = highestprob))
      }, simplify = FALSE))
    })
  } else if (type=="ngram"){
    bypointscale = lapply(unique_pointscales, function(k) {
      
      obs_thispointscale = obs[which(pointscales == k)]
      if(mcorder < 2){stop("mcorder should be an integer, 2 or greater")}
      mcorder = floor(mcorder)
      
      ngfit <- ngram(paste0(obs_thispointscale, collapse=""),mcorder,sep="")
      
      # simulate by babbling
      do.call(rbind, replicate(numperms, {
        str = strsplit(babble(ngfit, length(obs_thispointscale))," ")[[1]]
        as.numeric(str)
      }, simplify = FALSE))
      
    })
  }

  synth_disordered = do.call(cbind, bypointscale)
  # get into correct order
  synth = synth_disordered[, order(originalposition)]
  # put together
  together = rbind(obs, synth)
  return(together)
}

simHigherOrder <- function(hofit, numseq=10, highestprob=FALSE){
  
  vals <- vector("character")
  possvals <- names(hofit$X)
  order <- length(hofit$lambda)
  x <- matrix(vector("numeric"), nrow=0, ncol=length(possvals))
  
  # initial val in sequence
  vals[1] <- sample(possvals, 1, prob = hofit$X)
  x <- rbind(x,as.numeric(vals[1] == possvals))
  
  # additional vals in sequence
  previdx <- 1
  if(numseq > 1){
    for(t in 2:numseq){
      P <- 0
      for(h in 1:min(c(previdx, order))){
        P <- P + hofit$lambda[h]*hofit$Q[[h]]%*%x[t-h,]
      }
      if(!highestprob){
        newval <- sample(possvals, 1, prob = P)        
      } else {
        newval <- possvals[which.max(P)]
      }
      
      vals <- c(vals, newval)
      x <- rbind(x,as.numeric(newval == possvals))
      previdx <- previdx + 1      
    }
  }
  
  return(vals)
  
}