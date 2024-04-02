#' Impute midrange of point scale
#' 
#' Impute missing values with the midrange of the Likert-type categories.
#' For instance, if the column is on a scale of 1 to 5, replace missing values with "3";
#' if the column is on a scale of 1 to 4, replace missing values with "2.5".
#' 
#' @param x Likert-type data (matrix or dataframe). Smallest response category must be 1.
#' @param pointscales vector of integers indicating how many Likert-type response categories there are for each item. 
#' Length must match the number of columns in \code{data}.
#'
#' @return Matrix without missing values.
#' 
#' @noRd
imputepsm = function(x, pointscales) {
	completed = sapply(1:length(pointscales), function(j) {
		middle = (1+pointscales[j])/2
		ifelse(is.na(x[,j]), middle, x[,j])
	})
	return(completed)
}
