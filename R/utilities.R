#' Convert a value of 1 to a NULL value.
#'
#' \code{oneToNull} is a utility function that returns NULL when a value of
#' 0 or 1 is passed to it, otherwise it returns the original value.
#'
#' @param value The value to check.
#' @param convertOneToNULL TRUE to convert 1 to NULL.
#' @return NULL if value==1, otherwise value.

oneToNULL <- function(value, convertOneToNULL) {
  if(!convertOneToNULL) return(value)
  else if(is.null(value)) return(NULL)
  else if(value==0) return(NULL)
  else if(value==1) return(NULL)
  else return(value)
}

#' Check whether a text value is present.
#'
#' \code{isTextValue} is a utility function returns TRUE only when a text value
#' is present.  NULL, NA, character(0) and "" all return FALSE.
#'
#' @param value The value to check.
#' @return TRUE if a non-blank text value is present.

isTextValue <- function(value) {
  if(is.null(value)) return(FALSE)
  l <- length(value)
  if(l==0) return(FALSE)
  else if(l==1) {
    if(is.na(value)) return(FALSE)
    if(nchar(value)==0) return(FALSE)
    else return(TRUE)
  }
  else {
    if(sum(nchar(value), na.rm=TRUE)==0) return(FALSE)
    else return(TRUE)
  }
}

#' Check whether a numeric value is present.
#'
#' \code{isNumericValue} is a utility function returns TRUE only when a numeric value
#' is present.  NULL, NA, numeric(0) and integer(0) all return FALSE.
#'
#' @param value The value to check.
#' @return TRUE if a numeric value is present.

isNumericValue <- function(value) {
  if(is.null(value)) return(FALSE)
  l <- length(value)
  if(l==0) return(FALSE)
  else {
    if(length(value[is.na(value)])>0) return(FALSE)
    if(is.numeric(value)) return(TRUE)
    else return(FALSE)
  }
}

#' Check whether a text value is present in another text value.
#'
#' \code{containsText} is a utility function returns TRUE if one text value is
#' present in another.  Case sensitive.  If textToSearch is a vector, returns
#' TRUE if any element contains textToFind.
#'
#' @param textToSearch The value to be searched.
#' @param textToFind The value to find.
#' @return TRUE if the textToFind value is found.

containsText <- function(textToSearch, textToFind) {
  if(!isTextValue(textToSearch)) return(FALSE)
  if(!isTextValue(textToFind)) return(FALSE)
  r <- regexpr(textToFind, textToSearch, fixed=TRUE)
  return((length(r[r!=-1]))>0)
}
