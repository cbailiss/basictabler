#' A class that manages cell ranges (e.g. for merged cells).
#'
#' The TableCellRanges class contains a list of cell ranges and provides
#' basic utility methods such as finding intersecting ranges to support
#' the functioning of the BasicTable class.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that help
#'   manage cell ranges.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # TableCellRanges objects are never created outside of the BasicTable class.
#' # For examples of working with merged cells, see the Introduction vignette.
#' @field parentTable Owning table.
#' @field ranges A list of cell ranges.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new object to manage cell ranges.}
#'
#'   \item{\code{addRange(rTop, cLeft, rCount=NULL, cCount=NULL, rBottom=NULL,
#'   cRight=NULL)}}{Add a cell range to the list of cell ranges.}
#'   \item{\code{findIntersectingRange(rTop, cLeft, rCount=NULL, cCount=NULL,
#'   rBottom=NULL, cRight=NULL)}}{Find a cell range that intersects with the
#'   specified cell range.}
#'   \item{\code{deleteRange(r, c)}}{Delete the cell range covering the
#'   specified cell.}
#'   \item{\code{asList()}}{Get a list representation of this style.}
#'   \item{\code{asJSON()}}{Get a JSON representation of this style.}
#' }

TableCellRanges <- R6::R6Class("TableCellRanges",
  public = list(
    initialize = function(parentTable) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableCellRanges", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$new", "Creating new Table Cell Ranges...", list())
      private$p_ranges <- list()
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$new", "Created new Table Cell Ranges.")
    },
    addRange = function(rTop=NULL, cLeft=NULL, rCount=NULL, cCount=NULL, rBottom=NULL, cRight=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rTop, missing(rTop), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cLeft, missing(cLeft), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rCount, missing(rCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cCount, missing(cCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rBottom, missing(rBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cRight, missing(cRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$addRange", "Adding range...", list(rTop=rTop, cLeft=cLeft, rCount=rCount, cCount=cCount, rBottom=rBottom, cRight=cRight))
      range <- private$validateRange(rTop=rTop, cLeft=cLeft, rCount=rCount, cCount=cCount, rBottom=rBottom, cRight=cRight)
      if(!is.null(range)) {
        private$p_ranges[[length(private$p_ranges)+1]] <- range
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$addRange", "Added range.")
      return(invisible(range))
    },
    findIntersectingRange = function(rTop=NULL, cLeft=NULL, rCount=NULL, cCount=NULL, rBottom=NULL, cRight=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rTop, missing(rTop), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cLeft, missing(cLeft), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rCount, missing(rCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cCount, missing(cCount), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rBottom, missing(rBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cRight, missing(cRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$findIntersectingRange", "Searching for intersecting range...", list(rTop=rTop, cLeft=cLeft, rCount=rCount, cCount=cCount, rBottom=rBottom, cRight=cRight))
      findRange <- private$validateRange(rTop=rTop, cLeft=cLeft, rCount=rCount, cCount=cCount, rBottom=rBottom, cRight=cRight)
      if(is.null(findRange)) { return(invisible(NULL)) }
      if(length(private$p_ranges) > 0) {
        for(i in 1:length(private$p_ranges)) {
          compareRange <- private$p_ranges[[i]]
          # check for the inverse (i.e. where clearly no intersection) and then NOT that
          # i.e. is the findRange clearly above or clearly below of the compare range?
          rowIntersects <- !((findRange$rBottom < compareRange$rTop) || (findRange$rTop > compareRange$rBottom))
          # i.e. is the findRange clearly left or clearly right of the compare range?
          columnIntersects <- !((findRange$cRight < compareRange$cLeft) || (findRange$cLeft > compareRange$cRight))
          # the findRange intersects the compareRange if both the row and columns intersect
          if (rowIntersects && columnIntersects) { return(invisible(i)) }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$findIntersectingRange", "Searched for intersecting range.")
      return(invisible(NULL))
    },
    deleteRange = function(r=NULL, c=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$deleteRange", "Deleting range...", list(r=r, c=c))
      rangeIndex <- self$findIntersectingRange(rTop=r, cLeft=c, rCount=1, cCount=1)
      if(!is.null(rangeIndex)) {
        private$p_ranges[[rangeIndex]] <- NULL
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$deleteRange", "Deleted range.", list(rangeIndex=rangeIndex))
      return(invisible(!is.null(rangeIndex))) # returns TRUE if range found and deleted
    },
    asList = function() {
      lst <- list(
        ranges = private$p_ranges
      )
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
    ranges = function(value) { return(invisible(private$p_ranges)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_ranges = NULL,
    validateRange = function(rTop=NULL, cLeft=NULL, rCount=NULL, cCount=NULL, rBottom=NULL, cRight=NULL) {
      if(is.null(rTop)||is.na(rTop)) {
        stop(paste0("TableCellRanges$validateRange(): A value for the start row (rTop) must be specified."), call. = FALSE)
      }
      if(is.null(cLeft)||is.na(cLeft)) {
        stop(paste0("TableCellRanges$validateRange(): A value for the start column (cLeft) must be specified."), call. = FALSE)
      }
      rt <- rTop
      cl <- cLeft
      bRCountSpecified <- !(is.null(rCount)||is.na(rCount))
      bCCountSpecified <- !(is.null(cCount)||is.na(cCount))
      bBottomSpecified <- !(is.null(rBottom)||is.na(rBottom))
      bRightSpecified <- !(is.null(cRight)||is.na(cRight))
      if(bRCountSpecified) {
        rc <- rCount
        if(bBottomSpecified) { rb <- rBottom }
        else {
          rb <- rTop + rCount - 1
        }
      }
      else {
        if(bBottomSpecified) {
          rb <- rBottom
          rc <- rBottom - rTop + 1
        }
        else {
          stop(paste0("TableCellRanges$validateRange(): A value must be specified for either the end row (rBottom) or the row count (rCount)."), call. = FALSE)
        }
      }
      if(bCCountSpecified) {
        cc <- cCount
        if(bRightSpecified) { cr <- cRight }
        else {
          cr <- cLeft + cCount - 1
        }
      }
      else {
        if(bRightSpecified) {
          cr <- cRight
          cc <- cRight - cLeft + 1
        }
        else {
          stop(paste0("TableCellRanges$validateRange(): A value must be specified for either the end column (cRight) or the column count (cCount)."), call. = FALSE)
        }
      }
      if (rb < rt) {
        stop(paste0("TableCellRanges$validateRange(): Invalid row range: the start row (rTop) must be less than or equal to the end row (rBottom)."), call. = FALSE)
      }
      if ((rb - rt + 1) != rc) {
        stop(paste0("TableCellRanges$validateRange(): Invalid row range: the row count specified is not consistent with the start and end rows (i.e. rCount must equal rBottom - rTop + 1)."), call. = FALSE)
      }
      if (cr < cl) {
        stop(paste0("TableCellRanges$validateRange(): Invalid column range: the start column (cLeft) must be less than or equal to the end column (cRight)."), call. = FALSE)
      }
      if ((cr - cl + 1) != cc) {
        stop(paste0("TableCellRanges$validateRange(): Invalid column range: the column count specified is not consistent with the left and right columns (i.e. cCount must equal cRight - cLeft + 1)."), call. = FALSE)
      }
      range <- list(rTop=rt, cLeft=cl, rCount=rc, cCount=cc, rBottom=rb, cRight=cr)
      return(range)
    }
  )
)
