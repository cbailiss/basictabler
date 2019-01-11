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
#'   \item{\code{addRange(rFrom, cFrom, rSpan=NULL, cSpan=NULL, rTo=NULL,
#'   cTo=NULL)}}{Add a cell range to the list of cell ranges.}
#'   \item{\code{findIntersectingRange(rFrom, cFrom, rSpan=NULL, cSpan=NULL,
#'   rTo=NULL, cTo=NULL)}}{Find a cell range that intersects with the
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
    addRange = function(rFrom=NULL, cFrom=NULL, rSpan=NULL, cSpan=NULL, rTo=NULL, cTo=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rFrom, missing(rFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cFrom, missing(cFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rSpan, missing(rSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cSpan, missing(cSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "addRange", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$addRange", "Adding range...", list(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo))
      range <- private$validateRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(!is.null(range)) {
        private$p_ranges[[length(private$p_ranges)+1]] <- range
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$addRange", "Added range.")
      return(invisible(range))
    },
    findIntersectingRange = function(rFrom=NULL, cFrom=NULL, rSpan=NULL, cSpan=NULL, rTo=NULL, cTo=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rFrom, missing(rFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cFrom, missing(cFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rSpan, missing(rSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cSpan, missing(cSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCellRanges", "findIntersectingRange", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$findIntersectingRange", "Searching for intersecting range...", list(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo))
      findRange <- private$validateRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(is.null(findRange)) { return(invisible(NULL)) }
      if(length(private$p_ranges) > 0) {
        for(i in 1:length(private$p_ranges)) {
          compareRange <- private$p_ranges[[i]]
          # check for the inverse (i.e. where clearly no intersection) and then NOT that
          # i.e. is the findRange clearly above or clearly below of the compare range?
          rowIntersects <- !((findRange$rTo < compareRange$rFrom) || (findRange$rFrom > compareRange$rTo))
          # i.e. is the findRange clearly left or clearly right of the compare range?
          columnIntersects <- !((findRange$cTo < compareRange$cFrom) || (findRange$cFrom > compareRange$cTo))
          # the findRange intersects the compareRange if both the row and columns intersect
          if (rowIntersects && columnIntersects) { return(invisible(i)) }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$findIntersectingRange", "Searched for intersecting range.")
      return(invisible(NULL))
    },
    deleteRange = function(r=NULL, c=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$deleteRange", "Deleting range...", list(r=r, c=c))
      rangeIndex <- self$findIntersectingRange(rFrom=r, cFrom=c, rSpan=1, cSpan=1)
      if(!is.null(rangeIndex)) {
        private$p_ranges[[rangeIndex]] <- NULL
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$deleteRange", "Deleted range.", list(rangeIndex=rangeIndex))
      return(invisible(!is.null(rangeIndex))) # returns TRUE if range found and deleted
    },
    clear = function() {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$clear", "Clearing ranges...")
      private$p_ranges <- list()
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$clear", "Cleared ranges.")
    },
    updateAfterRowInsert = function(r=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterRowInsert", "Updating...", list(r=r))
      if(length(private$p_ranges) > 0) {
        for(i in 1:length(private$p_ranges)) {
          if(r<=private$p_ranges[[i]]$rFrom) {
            private$p_ranges[[i]]$rFrom <- private$p_ranges[[i]]$rFrom + 1
          }
          if(r<=private$p_ranges[[i]]$rTo) {
            private$p_ranges[[i]]$rTo <- private$p_ranges[[i]]$rTo + 1
            if((r>private$p_ranges[[i]]$rFrom)) {
              private$p_ranges[[i]]$rSpan <- private$p_ranges[[i]]$rSpan + 1
            }
          }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterRowInsert", "Updated.")
      return(invisible())
    },
    updateAfterRowDelete = function(r=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterRowDelete", "Updating...", list(r=r))
      if(length(private$p_ranges) > 0) {
        for(i in length(private$p_ranges):1) {
          if(r<private$p_ranges[[i]]$rFrom) {
            private$p_ranges[[i]]$rFrom <- private$p_ranges[[i]]$rFrom - 1
            private$p_ranges[[i]]$rTo <- private$p_ranges[[i]]$rTo - 1
          }
          else if(r==private$p_ranges[[i]]$rFrom) {
            if(private$p_ranges[[i]]$rSpan==1) {
              private$p_ranges[[i]] <- NULL
            }
            else {
              private$p_ranges[[i]]$rTo <- private$p_ranges[[i]]$rTo - 1
              private$p_ranges[[i]]$rSpan <- private$p_ranges[[i]]$rSpan - 1
            }
          }
          else if((r>private$p_ranges[[i]]$rFrom)&&(r<=private$p_ranges[[i]]$rTo)) {
            private$p_ranges[[i]]$rTo <- private$p_ranges[[i]]$rTo - 1
            private$p_ranges[[i]]$rSpan <- private$p_ranges[[i]]$rSpan - 1
          }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterRowDelete", "Updated.")
      return(invisible())
    },
    updateAfterColumnInsert = function(c=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterColumnInsert", "Updating...", list(c=c))
      if(length(private$p_ranges) > 0) {
        for(i in 1:length(private$p_ranges)) {
          if(c<=private$p_ranges[[i]]$cFrom) {
            private$p_ranges[[i]]$cFrom <- private$p_ranges[[i]]$cFrom + 1
          }
          if(c<=private$p_ranges[[i]]$cTo) {
            private$p_ranges[[i]]$cTo <- private$p_ranges[[i]]$cTo + 1
            if((c>private$p_ranges[[i]]$cFrom)) {
              private$p_ranges[[i]]$cSpan <- private$p_ranges[[i]]$cSpan + 1
            }
          }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterColumnInsert", "Updated.")
      return(invisible())
    },
    updateAfterColumnDelete = function(c=NULL) {
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterColumnDelete", "Updating...", list(c=c))
      if(length(private$p_ranges) > 0) {
        for(i in length(private$p_ranges):1) {
          if(c<private$p_ranges[[i]]$cFrom) {
            private$p_ranges[[i]]$cFrom <- private$p_ranges[[i]]$cFrom - 1
            private$p_ranges[[i]]$cTo <- private$p_ranges[[i]]$cTo - 1
          }
          else if(c==private$p_ranges[[i]]$cFrom) {
            if(private$p_ranges[[i]]$cSpan==1) {
              private$p_ranges[[i]] <- NULL
            }
            else {
              private$p_ranges[[i]]$cTo <- private$p_ranges[[i]]$cTo - 1
              private$p_ranges[[i]]$cSpan <- private$p_ranges[[i]]$cSpan - 1
            }
          }
          else if((c>private$p_ranges[[i]]$cFrom)&&(c<=private$p_ranges[[i]]$cTo)) {
            private$p_ranges[[i]]$cTo <- private$p_ranges[[i]]$cTo - 1
            private$p_ranges[[i]]$cSpan <- private$p_ranges[[i]]$cSpan - 1
          }
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCellRanges$updateAfterColumnDelete", "Updated.")
      return(invisible())
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
    validateRange = function(rFrom=NULL, cFrom=NULL, rSpan=NULL, cSpan=NULL, rTo=NULL, cTo=NULL) {
      if(is.null(rFrom)||is.na(rFrom)) {
        stop(paste0("TableCellRanges$validateRange(): A value for the start row (rFrom) must be specified."), call. = FALSE)
      }
      if(is.null(cFrom)||is.na(cFrom)) {
        stop(paste0("TableCellRanges$validateRange(): A value for the start column (cFrom) must be specified."), call. = FALSE)
      }
      rTop <- rFrom
      cLeft <- cFrom
      bRSpanSpecified <- !(is.null(rSpan)||is.na(rSpan))
      bCSpanSpecified <- !(is.null(cSpan)||is.na(cSpan))
      bRToSpecified <- !(is.null(rTo)||is.na(rTo))
      bCToSpecified <- !(is.null(cTo)||is.na(cTo))
      if(bRSpanSpecified) {
        rCount <- rSpan
        if(bRToSpecified) { rBottom <- rTo }
        else {
          rBottom <- rFrom + rSpan - 1
        }
      }
      else {
        if(bRToSpecified) {
          rBottom <- rTo
          rCount <- rTo - rFrom + 1
        }
        else {
          stop(paste0("TableCellRanges$validateRange(): A value must be specified for either the end row (rTo) or the row count (rSpan)."), call. = FALSE)
        }
      }
      if(bCSpanSpecified) {
        cCount <- cSpan
        if(bCToSpecified) { cRight <- cTo }
        else {
          cRight <- cFrom + cSpan - 1
        }
      }
      else {
        if(bCToSpecified) {
          cRight <- cTo
          cCount <- cTo - cFrom + 1
        }
        else {
          stop(paste0("TableCellRanges$validateRange(): A value must be specified for either the end column (cTo) or the column count (cSpan)."), call. = FALSE)
        }
      }
      if (rBottom < rTop) {
        stop(paste0("TableCellRanges$validateRange(): Invalid row range: the start row (rFrom) must be less than or equal to the end row (rTo)."), call. = FALSE)
      }
      if ((rBottom - rTop + 1) != rCount) {
        stop(paste0("TableCellRanges$validateRange(): Invalid row range: the row count specified is not consistent with the start and end rows (i.e. rSpan must equal rTo - rFrom + 1)."), call. = FALSE)
      }
      if (cRight < cLeft) {
        stop(paste0("TableCellRanges$validateRange(): Invalid column range: the start column (cFrom) must be less than or equal to the end column (cTo)."), call. = FALSE)
      }
      if ((cRight - cLeft + 1) != cCount) {
        stop(paste0("TableCellRanges$validateRange(): Invalid column range: the column count specified is not consistent with the left and right columns (i.e. cSpan must equal cTo - cFrom + 1)."), call. = FALSE)
      }
      range <- list(rFrom=rTop, cFrom=cLeft, rSpan=rCount, cSpan=cCount, rTo=rBottom, cTo=cRight)
      return(range)
    }
  )
)
