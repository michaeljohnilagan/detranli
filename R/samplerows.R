#' Sample rows from a dataset
#' 
#' Sample rows with replacement from a dataset.
#' 
#' @param n the number of rows to sample.
#' @param data the dataset to sample rows from.
#' 
#' @return The sample. Has the same class as \code{data}.
#' 
#' @seealso [base::sample()]
#' 
#' @export
#' 
#' @examples
#' set.seed(47)
#' samplerows(10, datasets::iris)
samplerows = function(n, data) {
	sampindex = sample(1:nrow(data), size=n, replace=TRUE)
	return(data[sampindex,,drop=FALSE])
}
