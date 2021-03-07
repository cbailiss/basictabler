#' R6 class that represents a cell in a table.
#'
#' @description
#' The `TableCell` class represents a cell in a table.  Both header cells and body
#' cells are represented by this class.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by using the functions in the table.
#' # It is not intended to be created by users outside of the table.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' cell1 <- tbl$cells$setCell(r=4, c=1, cellType="cell", rawValue=5)
#' cell2 <- tbl$cells$setCell(r=4, c=2, cellType="cell", rawValue=6)
#' tbl$renderTable()

TableCell <- R6::R6Class("TableCell",
  public = list(

   #' @description
   #' Create a new `TableCell` object.
   #' @param parentTable Owning table.
   #' @param rowNumber The row number of the cell.  1 = the first (i.e. top) data
   #'   row.
   #' @param columnNumber The column number of the cell.  1 = the first (i.e.
   #'   leftmost) data column.
   #' @param cellType One of the following values that specifies the type of cell:
   #' root, rowHeader, columnHeader, cell, total.  The cellType controls the
   #' default styling that is applied to the cell.
   #' @param visible `TRUE` or `FALSE` to specify whether the cell is rendered.
   #' @param rawValue The original unformatted value.
   #' @param formattedValue The formatted value (i.e. normally of character data
   #'   type).
   #' @param baseStyleName The name of the style applied to this cell (a character
   #'   value).  The style must exist in the TableStyles object associated with the
   #'   table.
   #' @param styleDeclarations A list containing CSS style declarations.
   #' @param asNBSP `TRUE` or `FALSE` to specify whether cells with no formatted
   #'   value be output as html nbsp.
   #' @return No return value.
   initialize = function(parentTable, rowNumber=NULL, columnNumber=NULL, cellType="cell", visible=TRUE, rawValue=NULL, formattedValue=NULL, baseStyleName=NULL, styleDeclarations=NULL, asNBSP=FALSE) {
     if(parentTable$argumentCheckMode > 0) {
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", cellType, missing(cellType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", asNBSP, missing(asNBSP), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCell$new", "Creating new TableCell",
                                   list(rowNumber=rowNumber, columnNumber=columnNumber))
     private$p_rowNumber <- rowNumber
     private$p_columnNumber <- columnNumber
     private$p_cellType <- cellType
     private$p_visible <- visible
     private$p_rawValue <- rawValue
     private$p_formattedValue <- as.character(formattedValue)
     private$p_asNBSP <- asNBSP
     private$p_baseStyleName <- baseStyleName
     if (!is.null(styleDeclarations)) private$p_style = private$p_parentTable$createInlineStyle(baseStyleName=baseStyleName, declarations=styleDeclarations)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCell$new", "Created new TableCell")
   },

   #' @description
   #' Set the cell location in the table.  Mainly exists for internal use.
   #' Typically used after rows/columns/cells are inserted or deleted (which
   #' shifts other cells).
   #' @param rowNumber The row number of the cell.  1 = the first (i.e. top) data
   #'   row.
   #' @param columnNumber The column number of the cell.  1 = the first (i.e.
   #'   leftmost) data column.
   #' @return No return value.
   updatePosition = function(rowNumber=NULL, columnNumber=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "updatePosition", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "updatePosition", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
     }
     private$p_rowNumber <- rowNumber
     private$p_columnNumber <- columnNumber
   },

   #' @description
   #' Stub only (ignore).
   #' @return No return value.
   getCopy = function() {
     copy <- list()
     return(invisible(copy))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       row=private$p_rowNumber,
       column=private$p_columnNumber,
       cellType=private$p_cellType,
       visible=private$p_visible,
       rawValue=private$p_rawValue,
       formattedValue=private$p_formattedValue,
       asNBSP=private$p_asNBSP
     )
     return(invisible(lst))
   },

   #' @description
   #' Return the contents of this object as JSON for debugging.
   #' @return A JSON representation of various object properties.
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(

   #' @field cellType One of the following values that specifies the type of cell:
   #' root, rowHeader, columnHeader, cell, total.  The cellType controls the
   #' default styling that is applied to the cell.
   cellType = function(value) {
     if(missing(value)) return(invisible(private$p_cellType))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "cellType", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       }
       private$p_cellType <- value
       return(invisible())
     }
   },

   #' @field rowNumber The row number of the cell.  1 = the first (i.e. top) data
   #'   row.
   rowNumber = function(value) { return(invisible(private$p_rowNumber)) },

   #' @field columnNumber The column number of the cell.  1 = the first (i.e.
   #'   leftmost) data column.
   columnNumber = function(value) { return(invisible(private$p_columnNumber)) },

   #' @field visible TRUE or FALSE to specify whether the cell is rendered.
   visible = function(value) {
     if(missing(value)) return(invisible(private$p_visible))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "visible", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       }
       private$p_visible <- value
       return(invisible())
     }
   },

   #' @field rawValue The original unformatted value.
   rawValue = function(value) {
     if(missing(value)) return(invisible(private$p_rawValue))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "rawValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       }
       private$p_rawValue <- value
       return(invisible())
     }
   },

   #' @field formattedValue The formatted value (i.e. normally of character data
   #'   type).
   formattedValue = function(value) {
     if(missing(value)) return(invisible(private$p_formattedValue))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "formattedValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       }
       private$p_formattedValue <- as.character(value)
       return(invisible())
     }
   },

   #' @field asNBSP TRUE or FALSE to specify whether cells with no formatted
   #'   value be output as html nbsp.
   asNBSP = function(value) {
     if(missing(value)) return(invisible(private$p_asNBSP))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "asNBSP", value, missing(value), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       }
       private$p_formattedValue <- value
       return(invisible())
     }
   },

   #' @field fValueOrNBSP For internal use by the renderers only.
   fValueOrNBSP = function(value) {
     hasValue <- FALSE
     if(length(private$p_formattedValue)>0)
     {
       hasValue <- (nchar(private$p_formattedValue)>0)
     }
     if((!hasValue) && (private$p_asNBSP==TRUE)) {
       return(invisible(htmltools::HTML("&nbsp;")))
     }
     else { return(invisible(private$p_formattedValue)) }
   },

   #' @field isMerged For internal use by the renderers only.
   isMerged = function(value) { # for internal use by the renderers only
     if(missing(value)) { return(invisible(private$p_isMerged)) }
     else {
       private$p_isMerged <- value
       return(invisible())
     }
   },

   #' @field isMergeRoot For internal use by the renderers only.
   isMergeRoot = function(value) { # for internal use by the renderers only
     if(missing(value)) { return(invisible(private$p_isMergeRoot)) }
     else {
       private$p_isMergeRoot <- value
       return(invisible())
     }
   },

   #' @field mergeIndex For internal use by the renderers only.
   mergeIndex = function(value) { # for internal use by the renderers only
     if(missing(value)) { return(invisible(private$p_mergeIndex)) }
     else {
       private$p_mergeIndex <- value
       return(invisible())
     }
   },

   #' @field baseStyleName The name of the style applied to this cell (a character
   #'   value).  The style must exist in the TableStyles object associated with the
   #'   table.
   baseStyleName = function(value) {
     if(missing(value)) { return(invisible(private$p_baseStyleName)) }
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "baseStyleName", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       }
       private$p_baseStyleName <- value
       return(invisible())
     }
   },

   #' @field style A TableStyle object that can apply overrides to the base style
   #'   for this cell.
   style = function(value) {
     if(missing(value)) { return(invisible(private$p_style)) }
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "style", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="TableStyle")
       }
       private$p_style <- value
       return(invisible())
     }
   }
  ),
  private = list(
    p_parentTable = NULL,             # an object ref (the single table instance)
    p_rowNumber = NULL,               # an integer
    p_columnNumber = NULL,            # an integer
    p_cellType = NULL,                # root, rowHeader, columnHeader, cell, total
    p_isMerged = NULL,                # TRUE if the cell is part of a merged cell (but only set by BasicTable$applyCellMerges())
    p_isMergeRoot = NULL,             # TRUE if the cell is the top-most, left-most cell in the merged cell (also only set by...)
    p_mergeIndex = NULL,              # the index of the merged cell (also only set by...)
    p_visible = NULL,
    p_rawValue = NULL ,               # a value (unique to this cell)
    p_formattedValue = NULL,          # a value (unique to this cell)
    p_asNBSP = NULL,                  # TRUE to output cells without a formatted value as &nbsp; in html
    p_baseStyleName = NULL,           # a string
    p_style = NULL                    # an object ref (may or may not be shared) to a TableStyle object
  )
)
