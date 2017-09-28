
#' Quickly build a basic table.
#'
#' The \code{qtbl} function builds a basic table with one line of R.
#'
#' @export
#' @param dataFrameOrMatrix The data frame or matrix containing the data to be
#'   displayed in the table.
#' @param columnNamesAsColumnHeaders TRUE to use the data frame column names
#'   as the column headers in the table.
#' @param explicitColumnHeaders A character vector of column headers.
#' @param rowNamesAsRowHeaders TRUE to use the data frame row names as the
#'   row headers in the table.
#' @param firstColumnAsRowHeaders TRUE to use the first column in the data
#'   frame as row headings.
#' @param explicitRowHeaders A character vector of row headers.
#' @param columnFormats A list containing format specifiers, each of which is
#'   either an sprintf() character value, a list of format() arguments or an
#'   R function that provides custom formatting logic.
#' @param ... Additional arguments, currently only argumentCheckMode.
#' @return A basic table.
#' @examples
#' qtbl(head(bhmsummary))
#' qtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")])
#' qtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")], columnNamesAsColumnHeaders=FALSE,
#'   explicitColumnHeaders=c("Week", "From", "To", "Trains", "On-Time"))
#'
qtbl <- function(dataFrameOrMatrix, columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                 rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL,
                 columnFormats=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qtbl", dataFrameOrMatrix, missing(dataFrameOrMatrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("data.frame", "matrix"))
  checkArgument(3, TRUE, "", "qtbl", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qtbl", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qtbl", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qtbl", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  tbl <- BasicTable$new()
  if("data.frame" %in% class(dataFrameOrMatrix)) {
    tbl$addData(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, firstColumnAsRowHeaders=firstColumnAsRowHeaders,
                explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  else if("matrix" %in% class(dataFrameOrMatrix)) {
    tbl$addMatrix(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  return(tbl)
}

#' Quickly render a basic table in HTML.
#'
#' The \code{qhpvt} function renders a basic table as a HTML widget with
#' one line of R.
#'
#' @export
#' @param dataFrameOrMatrix The data frame or matrix containing the data to be
#'   displayed in the table.
#' @param columnNamesAsColumnHeaders TRUE to use the data frame column names
#'   as the column headers in the table.
#' @param explicitColumnHeaders A character vector of column headers.
#' @param rowNamesAsRowHeaders TRUE to use the data frame row names as the
#'   row headers in the table.
#' @param firstColumnAsRowHeaders TRUE to use the first column in the data
#'   frame as row headings.
#' @param explicitRowHeaders A character vector of row headers.
#' @param columnFormats A list containing format specifiers, each of which is
#'   either an sprintf() character value, a list of format() arguments or an
#'   R function that provides custom formatting logic.
#' @param ... Additional arguments, currently only argumentCheckMode.
#' @return A basic table.
#' @examples
#' qhtbl(head(bhmsummary))
#' qhtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")])
#' qhtbl(bhmsummary[1:5, c("GbttWeekDate", "Origin", "Destination", "TrainCount",
#'   "OnTimeArrivals")], columnNamesAsColumnHeaders=FALSE,
#'   explicitColumnHeaders=c("Week", "From", "To", "Trains", "On-Time"))
#'
qhtbl <- function(dataFrameOrMatrix, columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                  rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL,
                  columnFormats=NULL, ...) {
  arguments <- list(...)
  checkArgument(3, TRUE, "", "qhtbl", dataFrameOrMatrix, missing(dataFrameOrMatrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("data.frame", "matrix"))
  checkArgument(3, TRUE, "", "qhtbl", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhtbl", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
  checkArgument(3, TRUE, "", "qhtbl", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
  checkArgument(3, TRUE, "", "qhtbl", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
  argumentCheckMode <- arguments$argumentCheckMode
  if(is.null(argumentCheckMode)) argumentCheckMode <- "auto"
  tbl <- BasicTable$new()
  if("data.frame" %in% class(dataFrameOrMatrix)) {
    tbl$addData(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, firstColumnAsRowHeaders=firstColumnAsRowHeaders,
                explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  else if("matrix" %in% class(dataFrameOrMatrix)) {
    tbl$addMatrix(dataFrameOrMatrix, columnNamesAsColumnHeaders=columnNamesAsColumnHeaders, explicitColumnHeaders=explicitColumnHeaders,
                rowNamesAsRowHeaders=rowNamesAsRowHeaders, explicitRowHeaders=explicitRowHeaders, columnFormats=columnFormats)
  }
  w <- tbl$renderTable()
  return(w)
}
