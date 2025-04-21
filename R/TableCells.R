#' R6 class that manages cells in a table.
#'
#' @description
#' The `TableCells` manages the `TableCell` objects that comprise a
#' `BasicTable` object.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link[R6]{R6Class}} object.
#' @examples
#' # This class should only be created by the table.
#' # It is not intended to be created outside of the table.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' cells <- tbl$cells
#' cells$setCell(r=4, c=1, cellType="cell", rawValue=5)
#' cells$setCell(r=4, c=2, cellType="cell", rawValue=6)
#' tbl$renderTable()

TableCells <- R6::R6Class("TableCells",
  public = list(

   #' @description
   #' Create a new `TableCells` object.
   #' @param parentTable Owning table.
   #' @return No return value.
   initialize = function(parentTable=NULL) {
     if(parentTable$argumentCheckMode > 0) {
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCells", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
     }
     private$p_parentTable <- parentTable
     private$p_rows <- NULL
     private$p_columnCount <- 0
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$new", "Creating new TableCells...")
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$new", "Created new TableCells.")
   },

   #' @description
   #' Clear all cells from the table.
   #' @return No return value.
   reset = function() {
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$resetCells", "Resetting cells...")
     private$p_rows <- NULL
     private$p_columnCount <- 0
     private$p_parentTable$mergedCells$clear()
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$resetCells", "Reset cells.")
   },

   #' @description
   #' Retrieve a specific `TableCell` object at the specified location in the
   #' table.
   #' @param r The row number of the cell to retrieve.
   #' @param c The column number of the cell to retrieve.
   #' @return A `TableCell` object.
   getCell = function(r=NULL, c=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
     }
     if(r < 1)
       stop(paste0("TableCells$getCell(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("TableCells$getCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("TableCells$getCell(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("TableCells$getCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     if(length(private$p_rows[[r]]) < c) return(invisible(NULL))
     return(invisible(private$p_rows[[r]][[c]]))
   },

   #' @description
   #' Retrieve the value of a specific `TableCell` object at the specified
   #' location in the table.
   #' @param r The row number of the cell value to retrieve.
   #' @param c The column number of the cell value to retrieve.
   #' @param formattedValue `TRUE` to retrieve the formatted (character) cell
   #'   value, `FALSE` (default) to retrieve the raw cell value (typically
   #'   numeric).
   #' @return The value of the cell.
   getValue = function(r=NULL, c=NULL, formattedValue=FALSE) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getValue", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getValue", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getValue", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     cell <- self$getCell(r, c)
     if(is.null(cell)) return(invisible())
     if(formattedValue) return(invisible(cell$formattedValue))
     else return(invisible(cell$rawValue))
   },

   #' @description
   #' Get a vector or list of the values in a row for the entire row or a subset
   #' of columns.
   #' @param rowNumber The row number to retrieve the values for (a single row
   #'   number).
   #' @param columnNumbers The column numbers of the cell value to retrieve (can
   #'   be a vector of column numbers).
   #' @param formattedValue `TRUE` to retrieve the formatted (character) cell
   #'   value, `FALSE` (default) to retrieve the raw cell value (typically
   #'   numeric).
   #' @param asList `TRUE` to retrieve the values as a list, `FALSE` (default)
   #'   to retrieve the values as a vector.
   #' @param rebase `TRUE` to rebase the list/vector so that the first element
   #'   is at index 1, `FALSE` to retain the original column numbers.
   #' @return A vector or list of the cell values.
   getRowValues = function(rowNumber=NULL, columnNumbers=NULL, formattedValue=FALSE, asList=FALSE, rebase=TRUE) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getRowValues", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getRowValues", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getRowValues", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getRowValues", asList, missing(asList), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getRowValues", rebase, missing(rebase), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(is.null(columnNumbers)) columnNumbers <- 1:self$columnCount
     rv <- NULL
     if(asList) {
       rv <- list()
       cMin <- min(columnNumbers)
       for(c in columnNumbers){
         v <- self$getValue(rowNumber, c, formattedValue=formattedValue)
         if(rebase) x <- c - cMin + 1
         else x <- c
         rv[[x]] <- v
       }
     }
     else {
       cMin <- min(columnNumbers)
       for(c in columnNumbers){
         v <- self$getValue(rowNumber, c, formattedValue=formattedValue)
         if(length(v)==0) v <- NA
         if(rebase) x <- c - cMin + 1
         else x <- c
         rv[x] <- v
       }
     }
     return(invisible(rv))
   },

   #' @description
   #' Get a vector or list of the values in a column for the entire column or a
   #' subset of rows.
   #' @param columnNumber The column number to retrieve the values for (a single
   #'   column number).
   #' @param rowNumbers The row numbers of the cell value to retrieve (can be a
   #'   vector of row numbers).
   #' @param formattedValue `TRUE` to retrieve the formatted (character) cell
   #'   value, `FALSE` (default) to retrieve the raw cell value (typically
   #'   numeric).
   #' @param asList `TRUE` to retrieve the values as a list, `FALSE` (default)
   #'   to retrieve the values as a vector.
   #' @param rebase `TRUE` to rebase the list/vector so that the first element
   #'   is at index 1, `FALSE` to retain the original row numbers.
   #' @return A vector or list of the cell values.
   getColumnValues = function(columnNumber=NULL,rowNumbers=NULL, formattedValue=FALSE, asList=FALSE, rebase=TRUE) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getColumnValues", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getColumnValues", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getColumnValues", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getColumnValues", asList, missing(asList), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getColumnValues", rebase, missing(rebase), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(is.null(rowNumbers)) rowNumbers <- 2:self$rowCount
     cv <- NULL
     if(asList) {
       cv <- list()
       rMin <- min(rowNumbers)
       for(r in rowNumbers){
         v <- self$getValue(r, columnNumber, formattedValue=formattedValue)
         if(rebase) x <- r - rMin + 1
         else x <- r
         cv[[x]] <- v
       }
     }
     else {
       rMin <- min(rowNumbers)
       for(r in rowNumbers){
         v <- self$getValue(r, columnNumber, formattedValue=formattedValue)
         if(length(v)==0) v <- NA
         if(rebase) x <- r - rMin + 1
         else x <- r
         cv[x] <- v
       }
     }
     return(invisible(cv))
   },

   #' @description
   #' Create a cell in the table and set the details of the cell.
   #' @param r The row number of the cell.
   #' @param c The column number of the cell.
   #' @param cellType The type of the cell - must be one of the following
   #'   values:  root, rowHeader, columnHeader, cell, total.
   #' @param rawValue The raw value of the cell - typically a numeric value.
   #' @param formattedValue The formatted value of the cell - typically a
   #'   character value.
   #' @param visible `TRUE` (default) to specify that the cell is visible,
   #'   `FALSE` to specify that the cell will be invisible.
   #' @param baseStyleName The name of a style from the table theme that will be
   #'   used to style this cell.
   #' @param styleDeclarations A list of CSS style definitions.
   #' @param rowSpan A number greater than 1 to indicate that this cell is
   #'   merged with cells below. `NULL` (default) or 1 means the cell is not
   #'   merged across rows.
   #' @param colSpan A number greater than 1 to indicate that this cell is
   #'   merged with cells to the right. `NULL` (default) or 1 means the cell is
   #'   not merged across columns.
   #' @return A vector or list of the cell values.
   setCell = function(r=NULL, c=NULL, cellType="cell", rawValue=NULL, formattedValue=NULL, visible=TRUE, baseStyleName=NULL, styleDeclarations=NULL, rowSpan=NULL, colSpan=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", cellType, missing(cellType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", rowSpan, missing(rowSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", colSpan, missing(colSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
     }
     if(missing(formattedValue)) formattedValue <- rawValue
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, rawValue=rawValue, formattedValue=formattedValue, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations)
     self$moveCell(r, c, cell)
     if((!is.null(rowSpan))||(!is.null(colSpan))) {
       if(is.null(rowSpan)) { rowSpan <- 1 }
       if(is.null(colSpan)) { colSpan <- 1 }
       if((rowSpan>1)||(colSpan>1)) { private$p_parentTable$mergeCells(rFrom=r, cFrom=c, rSpan=rowSpan, cSpan=colSpan) }
     }
     return(invisible(cell))
   },

   #' @description
   #' Create an empty cell in the table and set the details of the cell.
   #' @param r The row number of the cell.
   #' @param c The column number of the cell.
   #' @param cellType The type of the cell - must be one of the following
   #'   values:  root, rowHeader, columnHeader, cell, total.
   #' @param visible `TRUE` (default) to specify that the cell is visible,
   #'   `FALSE` to specify that the cell will be invisible.
   #' @param baseStyleName The name of a style from the table theme that will be
   #'   used to style this cell.
   #' @param styleDeclarations A list of CSS style definitions.
   #' @param rowSpan A number greater than 1 to indicate that this cell is
   #'   merged with cells below. `NULL` (default) or 1 means the cell is not
   #'   merged across rows.
   #' @param colSpan A number greater than 1 to indicate that this cell is
   #'   merged with cells to the right. `NULL` (default) or 1 means the cell is
   #'   not merged across columns.
   #' @param asNBSP `TRUE` if the cell should be rendered as &nbsp; in HTML,
   #'   `FALSE` (default) otherwise.
   #' @return A vector or list of the cell values.
   setBlankCell = function(r=NULL, c=NULL, cellType="cell", visible=TRUE, baseStyleName=NULL, styleDeclarations=NULL, rowSpan=NULL, colSpan=NULL, asNBSP=FALSE) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", cellType, missing(cellType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", rowSpan, missing(rowSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", colSpan, missing(colSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setBlankCell", asNBSP, missing(asNBSP), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("logical"))
     }
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, visible=visible, rawValue=NULL, formattedValue=NULL, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations, asNBSP=asNBSP)
     self$moveCell(r, c, cell)
     if((!is.null(rowSpan))||(!is.null(colSpan))) {
       rs <- rowSpan
       cs <- colSpan
       if(is.null(rowSpan)) { rowSpan <- 1 }
       if(is.null(colSpan)) { colSpan <- 1 }
       if((rowSpan>1)||(colSpan>1)) { private$p_parentTable$mergeCells(rFrom=r, cFrom=c, rSpan=rowSpan, cSpan=colSpan) }
     }
     return(invisible(cell))
   },

   #' @description
   #' Replace the `TableCell` object at the specified
   #' location in the table with a blank cell.
   #' @param r The row number of the cell value to delete
   #' @param c The column number of the cell value to delete
   #' @return The `TableCell` object that is the new blank cell.
   deleteCell = function(r=NULL, c=NULL) {
     cell <- self$setBlankCell(r, c, visible=FALSE)
     return(invisible(cell))
   },

   #' @description
   #' Update the value of a cell in the table.
   #' @param r The row number of the cell.
   #' @param c The column number of the cell.
   #' @param rawValue The raw value of the cell - typically a numeric value.
   #' @param formattedValue The formatted value of the cell - typically a
   #'   character value.
   #' @return No return value.
   setValue = function(r=NULL, c=NULL, rawValue=NULL, formattedValue=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setValue", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setValue", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setValue", rawValue, missing(rawValue), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setValue", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
     }
     cell <- self$getCell(r=r, c=c)
     cell$rawValue <- rawValue
     if(missing(formattedValue)) cell$formattedValue <- rawValue
     else cell$formattedValue <- formattedValue
   },

   #' @description
   #' Create multiple cells in one row of a table.
   #' @param rowNumber The row number where the cells will be created.
   #' @param startAtColumnNumber The column number to start generating cells at.
   #'   Default value 1.
   #' @param cellTypes The types of the cells - either a single value or a
   #'   vector of the same length as rawValues.  Each cellType must be one of
   #'   the following values:  root, rowHeader, columnHeader, cell, total.
   #' @param rawValues A vector or list of values.  A cell will be generated in
   #'   the table for each element in the vector/list.
   #' @param formattedValues A vector or list of formatted values.  Must be
   #'   either `NULL`, a single value or a vector/list of the same length as
   #'   rawValues.
   #' @param formats A vector or list of formats.  Must be either `NULL`, a
   #'   single value or a vector/list of the same length as rawValues.
   #' @param visiblity A logical vector.  Must be either a single logical value
   #'   or a vector/list of the same length as rawValues.
   #' @param baseStyleNames A character vector.  Must be either a single style
   #'   name (from the table theme) or a vector of style names of the same
   #'   length as rawValues.
   #' @param fmtFuncArgs A list that is length 1 or the same length as the
   #'   number of columns in the row, where each list element specifies a list
   #'   of arguments to pass to custom R format functions.
   #' @return No return value.
   setRow = function(rowNumber=NULL, startAtColumnNumber=1, cellTypes="cell", rawValues=NULL, formattedValues=NULL, formats=NULL, visiblity=TRUE, baseStyleNames=NULL, fmtFuncArgs=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", startAtColumnNumber, missing(startAtColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", cellTypes, missing(cellTypes), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", rawValues, missing(rawValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", formattedValues, missing(formattedValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", formats, missing(formats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", visiblity, missing(visiblity), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
     }
     if(length(rowNumber)!=1) stop(paste0("TableCells$setRow(): rowNumber (length ", length(rowNumber), ") must be one value."), call. = FALSE)
     if(length(startAtColumnNumber)!=1) stop(paste0("TableCells$setRow(): startAtColumnNumber (length ", length(startAtColumnNumber), ") must be one value."), call. = FALSE)
     cellCount <- length(rawValues)
     if(length(cellCount)==0) stop("TableCells$setRow(): At least one rawValue must be supplied.")
     if((length(cellTypes)>1)&&(length(cellTypes)!=cellCount)) stop("TableCells$setRow(): cellTypes must be either one value or the same length as rawValues.")
     if((length(formattedValues)>1)&&(length(formattedValues)!=cellCount)) stop("TableCells$setRow(): formattedValues must be either NULL, one value or the same length as rawValues.")
     if((length(formats)>1)&&(length(formats)!=cellCount)) stop("TableCells$setRow(): formats must be either NULL, one value or the same length as rawValues.")
     if((length(visiblity)>1)&&(length(visiblity)!=cellCount)) stop("TableCells$setRow(): visiblity must be either one value or the same length as rawValues.")
     if((length(baseStyleNames)>1)&&(length(baseStyleNames)!=cellCount)) stop("TableCells$setRow(): baseStyleNames must be either one value or the same length as rawValues.")
     c <- startAtColumnNumber - 1
     for(x in 1:length(rawValues)) {
       c <- c + 1
       if(is.list(rawValues)) v <- rawValues[[x]]
       else v <- rawValues[x]
       if(length(cellTypes)==1) cellType <- cellTypes[1]
       else cellType <- cellTypes[x]
       if(length(formats)==1) format=formats[[1]]
       else if(length(formats)>1) format=formats[[x]]
       if(length(fmtFuncArgs)==1) fmtFuncArg1=fmtFuncArgs[[1]]
       else if(length(fmtFuncArgs)>1) fmtFuncArg1=fmtFuncArgs[[x]]
       else fmtFuncArg1 <- NULL
       if(length(formattedValues)==0) {
         if(!is.null(format)) formattedValue <- private$p_parentTable$formatValue(value=v, format=format, fmtFuncArgs=fmtFuncArg1)
         else formattedValue <- v
       }
       else if(is.list(formattedValues)) {
         if(length(formattedValues)==1) formattedValue <- formattedValues[[1]]
         else formattedValue <- formattedValues[[x]]
       }
       else {
         if(length(formattedValues)==1) formattedValue <- formattedValues[1]
         else formattedValue <- formattedValues[x]
       }
       if(length(visiblity)==1) visible <- visiblity[1]
       else visible <- visiblity[x]
       if(length(baseStyleNames)==1) baseStyleName <- baseStyleNames[1]
       else baseStyleName <- baseStyleNames[x]
       self$setCell(r=rowNumber, c=c, cellType=cellType, rawValue=v, formattedValue=formattedValue, visible=visible, baseStyleName=baseStyleName)
     }
   },

   #' @description
   #' Create multiple cells in one column of a table.
   #' @param columnNumber The column number where the cells will be created.
   #' @param startAtRowNumber The row number to start generating cells at.
   #'   Default value 2.
   #' @param cellTypes The types of the cells - either a single value or a
   #'   vector of the same length as rawValues.  Each cellType must be one of
   #'   the following values:  root, rowHeader, columnHeader, cell, total.
   #' @param rawValues A vector or list of values.  A cell will be generated in
   #'   the table for each element in the vector/list.
   #' @param formattedValues A vector or list of formatted values.  Must be
   #'   either `NULL`, a single value or a vector of the same length as
   #'   rawValues.
   #' @param formats A vector or list of formats.  Must be either `NULL`, a
   #'   single value or a vector of the same length as rawValues.
   #' @param visiblity A logical vector.  Must be either a single logical value
   #'   or a vector of the same length as rawValues.
   #' @param baseStyleNames A character vector.  Must be either a single style
   #'   name (from the table theme) or a vector of style names of the same
   #'   length as rawValues.
   #' @param fmtFuncArgs A list that is length 1 or the same length as the
   #'   number of rows in the column, where each list element specifies a list
   #'   of arguments to pass to custom R format functions.
   #' @return No return value.
   setColumn = function(columnNumber=NULL, startAtRowNumber=2, cellTypes="cell", rawValues=NULL, formattedValues=NULL, formats=NULL, visiblity=TRUE, baseStyleNames=NULL, fmtFuncArgs=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", startAtRowNumber, missing(startAtRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", cellTypes, missing(cellTypes), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", rawValues, missing(rawValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", formattedValues, missing(formattedValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", formats, missing(formats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", visiblity, missing(visiblity), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
     }
     if(length(columnNumber)!=1) stop(paste0("TableCells$setColumn(): columnNumber (length ", length(columnNumber), ") must be one value."), call. = FALSE)
     if(length(startAtRowNumber)!=1) stop(paste0("TableCells$setColumn(): startAtRowNumber (length ", length(startAtRowNumber), ") must be one value."), call. = FALSE)
     cellCount <- length(rawValues)
     if(length(cellCount)==0) stop("TableCells$setColumn(): At least one rawValue must be supplied.")
     if((length(cellTypes)>1)&&(length(cellTypes)!=cellCount)) stop("TableCells$setColumn(): cellTypes must be either one value or the same length as rawValues.")
     if((length(formattedValues)>1)&&(length(formattedValues)!=cellCount)) stop("TableCells$setColumn(): formattedValues must be either NULL, one value or the same length as rawValues.")
     if((length(formats)>1)&&(length(formats)!=cellCount)) stop("TableCells$setColumn(): formats must be either NULL, one value or the same length as rawValues.")
     if((length(visiblity)>1)&&(length(visiblity)!=cellCount)) stop("TableCells$setColumn(): visiblity must be either one value or the same length as rawValues.")
     if((length(baseStyleNames)>1)&&(length(baseStyleNames)!=cellCount)) stop("TableCells$setColumn(): baseStyleNames must be either one value or the same length as rawValues.")
     r <- startAtRowNumber - 1
     for(x in 1:length(rawValues)) {
       r <- r + 1
       if(is.list(rawValues)) v <- rawValues[[x]]
       else v <- rawValues[x]
       if(length(cellTypes)==1) cellType <- cellTypes[1]
       else cellType <- cellTypes[x]
       if(length(formats)==1) format=formats[[1]]
       else if(length(formats)>1) format=formats[[x]]
       if(length(fmtFuncArgs)==1) fmtFuncArg1=fmtFuncArgs[[1]]
       else if(length(fmtFuncArgs)>1) fmtFuncArg1=fmtFuncArgs[[x]]
       else fmtFuncArg1 <- NULL
       if(length(formattedValues)==0) {
         if(!is.null(format)) formattedValue <- private$p_parentTable$formatValue(value=v, format=format, fmtFuncArgs=fmtFuncArg1)
         else formattedValue <- v
       }
       else if(is.list(formattedValues)) {
         if(length(formattedValues)==1) formattedValue <- formattedValues[[1]]
         else formattedValue <- formattedValues[[x]]
       }
       else {
         if(length(formattedValues)==1) formattedValue <- formattedValues[1]
         else formattedValue <- formattedValues[x]
       }
       if(length(visiblity)==1) visible <- visiblity[1]
       else visible <- visiblity[x]
       if(length(baseStyleNames)==1) baseStyleName <- baseStyleNames[1]
       else baseStyleName <- baseStyleNames[x]
       self$setCell(r=r, c=columnNumber, cellType=cellType, rawValue=v, formattedValue=formattedValue, visible=visible, baseStyleName=baseStyleName)
     }
   },

   #' @description
   #' Enlarge a table to the specified size.
   #' @param rowCount The number of rows in the enlarged table.
   #' @param columnCount The number of columns in the enlarged table.
   #' @return No return value.
   extendCells = function(rowCount=NULL, columnCount=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "extendCells", rowCount, missing(rowCount), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "extendCells", columnCount, missing(columnCount), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
     }
     rFrom <- max(1, self$rowCount)
     cFrom <- max(1, self$columnCount)
     rTo <- max(self$rowCount, rowCount)
     cTo <- max(self$columnCount, columnCount)
     for(r in 1:rTo) {
       if(r > length(private$p_rows)) private$p_rows[[r]] <- list()
       for(c in 1:cTo) {
         addCell <- FALSE
         if(c > length(private$p_rows[[r]])) addCell <- TRUE
         else if(is.null(private$p_rows[[r]][[c]])) addCell <- TRUE
         if(addCell) {
          private$p_rows[[r]][[c]] <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType="cell", visible=FALSE, rawValue=NULL, formattedValue=NULL)
         }
       }
     }
     private$p_columnCount <- cTo
     return(invisible())
   },

   #' @description
   #' Move a table cell to a different location in the table.
   #' @param r The new row number to move the cell to.
   #' @param c The new column number to move the cell to.
   #' @param cell The `TableCell` object to move.
   #' @return No return value.
   moveCell = function(r=NULL, c=NULL, cell=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "moveCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "moveCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "moveCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="TableCell")
     }
     if(r < 1)
       stop(paste0("TableCells$moveCell(): r (", r, ") must be greater than or equal to 1."), call. = FALSE)
     if(c < 1)
       stop(paste0("TableCells$moveCell(): c (", c, ") must be greater than or equal to 1."), call. = FALSE)
     if((r > self$rowCount)||(c > self$columnCount)) self$extendCells(r, c)
     private$p_rows[[r]][[c]] <- cell
     cell$updatePosition(r, c)
     return(invisible())
   },

   #' @description
   #' Insert a new row in the table at the specified row number and shift existing cells on/below this row down by one row.
   #' @param rowNumber The row number where the new row is to be inserted.
   #' @param insertBlankCells `TRUE` (default) to insert blank cells in the new row, `FALSE` to create no cells in the new row.
   #' @param headerCells The number of header cells to create at the start of the row.  Default value 1.
   #' @param totalCells The number of total cells to create at the end of the row.  Default value 0.
   #' @return No return value.
   insertRow = function(rowNumber=NULL, insertBlankCells=TRUE, headerCells=1, totalCells=0) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertRow", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertRow", insertBlankCells, missing(insertBlankCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertRow", headerCells, missing(headerCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=0)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertRow", totalCells, missing(totalCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=0)
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertRow", "Inserting row...")
     if(rowNumber<=self$rowCount) {
       for(r in self$rowCount:rowNumber) {
         for(c in 1:self$columnCount) {
           cell <- self$getCell(r, c)
           self$moveCell(r + 1, c, cell)
         }
       }
     }
     if(insertBlankCells) {
       for(c in 1:self$columnCount) {
         cellType <- "cell"
         if(c<=headerCells) { cellType <- "rowHeader" }
         else if(c>=(self$columnCount-totalCells+1))  { cellType <- "total" }
         self$setBlankCell(rowNumber, c, cellType=cellType)
       }
     }
     private$p_parentTable$mergedCells$updateAfterRowInsert(rowNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertRow", "Inserted row.")
     return(invisible())
   },

   #' @description
   #' Delete the row in the table at the specified row number and shift existing cells below this row up by one row.
   #' @param rowNumber The row number of the row to be deleted.
   #' @return No return value.
   deleteRow = function(rowNumber=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "deleteRow", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$deleteRow", "Deleting row...")
     if(rowNumber < self$rowCount) {
       for(r in rowNumber:(self$rowCount - 1)) {
         for(c in 1:self$columnCount) {
           cell <- self$getCell(r + 1, c)
           self$moveCell(r, c, cell)
         }
       }
     }
     private$p_rows[[self$rowCount]] <- NULL # can set last cell in the list to NULL to shorten the array
     private$p_parentTable$mergedCells$updateAfterRowDelete(rowNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$deleteRow", "Deleted row.")
     return(invisible())
   },

   #' @description
   #' Insert a new column in the table at the specified column number and shift existing cells in/to the right of this column across by one row.
   #' @param columnNumber The column number where the new column is to be inserted.
   #' @param insertBlankCells `TRUE` (default) to insert blank cells in the new column, `FALSE` to create no cells in the new column
   #' @param headerCells The number of header cells to create at the top of the column.  Default value 1.
   #' @param totalCells The number of total cells to create at the bottom of the column.  Default value 0.
   #' @return No return value.
   insertColumn = function(columnNumber=NULL, insertBlankCells=TRUE, headerCells=1, totalCells=0) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertColumn", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertColumn", insertBlankCells, missing(insertBlankCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertColumn", headerCells, missing(headerCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=0)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertColumn", totalCells, missing(totalCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=0)
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertColumn", "Inserting column...")
     if(columnNumber<=self$columnCount) {
       for(c in self$columnCount:columnNumber) {
         for(r in 1:self$rowCount) {
           cell <- self$getCell(r, c)
           self$moveCell(r, c + 1, cell)
         }
       }
     }
     if(insertBlankCells) {
       for(r in 1:self$rowCount) {
         cellType <- "cell"
         if(r<=headerCells) { cellType <- "columnHeader" }
         else if(r>=(self$rowCount-totalCells+1))  { cellType <- "total" }
         self$setBlankCell(r, columnNumber)
       }
     }
     private$p_parentTable$mergedCells$updateAfterColumnInsert(columnNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertColumn", "Inserted column")
     return(invisible())
   },

   #' @description
   #' Delete the column in the table at the specified column number and shift existing cells to the right of this column to the left by one column.
   #' @param columnNumber The column number of the column to be deleted.
   #' @return No return value.
   deleteColumn = function(columnNumber=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "deleteColumn", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$deleteColumn", "Deleting column...")
     if(columnNumber < self$columnCount) {
       for(c in columnNumber:(self$columnCount - 1)) {
         for(r in 1:self$rowCount) {
           cell <- self$getCell(r, c + 1)
           self$moveCell(r, c, cell)
         }
       }
     }
     for(r in 1:self$rowCount) {
       private$p_rows[[r]][[self$columnCount]] <- NULL # can set last cell in the list to NULL to shorten the array
     }
     private$p_columnCount <- private$p_columnCount - 1
     private$p_parentTable$mergedCells$updateAfterColumnDelete(columnNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$deleteColumn", "Deleted column.")
     return(invisible())
   },

   #' @description
   #' Retrieve cells by a combination of row and/or column numbers.
   #' See the "Finding and Formatting" vignette for graphical examples.
   #' @details
   #' When `specifyCellsAsList=TRUE` (the default):\cr
   #' Get one or more rows by specifying the row numbers as a vector as
   #' the rowNumbers argument and leaving the columnNumbers argument set
   #' to the default value of `NULL`, or\cr
   #' Get one or more columns by specifying the column numbers as a vector
   #' as the columnNumbers argument and leaving the rowNumbers argument
   #' set to the default value of `NULL`, or\cr
   #' Get one or more individual cells by specifying the cellCoordinates
   #' argument as a list of vectors of length 2, where each element in the
   #' list is the row and column number of one cell,\cr
   #' e.g. `list(c(1, 2), c(3, 4))` specifies two cells, the first located
   #' at row 1, column 2 and the second located at row 3, column 4.\cr
   #' When `specifyCellsAsList=FALSE`:\cr
   #' Get one or more rows by specifying the row numbers as a vector as the
   #' rowNumbers argument and leaving the columnNumbers argument set to the
   #' default value of `NULL`, or\cr
   #' Get one or more columns by specifying the column numbers as a vector
   #' as the columnNumbers argument and leaving the rowNumbers argument set
   #' to the default value of `NULL`, or\cr
   #' Get one or more cells by specifying the row and column numbers as vectors
   #' for the rowNumbers and columnNumbers arguments, or\cr
   #' a mixture of the above, where for entire rows/columns the element in the
   #' other vector is set to `NA`, e.g. to retrieve whole rows, specify the row
   #' numbers as the rowNumbers but set the corresponding elements in the
   #' columnNumbers vector to `NA`.
   #' @param specifyCellsAsList `TRUE`/`FALSE` to specify how cells are retrieved.
   #' Default `TRUE`. More information is provided in the details section.
   #' @param rowNumbers A vector of row numbers that specify the rows or
   #' cells to retrieve.
   #' @param columnNumbers A vector of row numbers that specify the columns
   #' or cells to retrieve.
   #' @param cellCoordinates A list of two-element vectors that specify the
   #' coordinates of cells to retrieve.  Ignored when `specifyCellsAsList=FALSE`.
   #' @param excludeEmptyCells Default `FALSE`.  Specify `TRUE` to exclude empty
   #' cells.
   #' @param matchMode Either "simple" (default) or "combinations"\cr
   #' "simple" specifies that row and column arguments are considered separately
   #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
   #' row 1 and all cells in column 2.\cr
   #' "combinations" specifies that row and column arguments are considered together
   #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
   #' cell single at location (1, 2).\cr
   #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
   #' affected by the match mode.  All other arguments are not.
   #' @return A list of `TableCell` objects.
   getCells = function(specifyCellsAsList=TRUE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL, excludeEmptyCells=FALSE, matchMode="simple") {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", excludeEmptyCells, missing(excludeEmptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getCells", "Getting cells...")
     if(specifyCellsAsList==FALSE) {
       # NA is allowed in rowNumbers or columnNumbers
       # cells are specified as in the rowNumbers and columnNumbers
       if((!is.null(cellCoordinates))&&(length(cellCoordinates)>0)) {
         stop("TableCells$getCells():  When specifyCellsAsList=FALSE, cell coordinates should be specified using the rowNumbers and columnNumbers arguments.  Please see the \"Finding and Formatting\" vignette for more details.", call. = FALSE)
       }
       # pre-processing to put the arguments into the same structure as-if specifyCellsAsList==TRUE
       newRowNumbers <- NULL
       newColumnNumbers <- NULL
       newCellCoordinates <- list()
       rmax <- length(rowNumbers)
       cmax <- length(columnNumbers)
       imax <- max(rmax, cmax)
       if(imax>0) {
         for(i in 1:imax) {
           if((i<=rmax)&&(i<=cmax)) {
             if(rowNumbers[i] %in% NA) {
               if(columnNumbers[i] %in% NA) next
               else newColumnNumbers[length(newColumnNumbers)+1] <- columnNumbers[i]
             }
             else if(columnNumbers[i] %in% NA) newRowNumbers[length(newRowNumbers)+1] <- rowNumbers[i]
             else newCellCoordinates[[length(newCellCoordinates)+1]] <- c(rowNumbers[i], columnNumbers[i])
           }
           else if(i<=rmax) {
             if(!(rowNumbers[i] %in% NA)) newRowNumbers[length(newRowNumbers)+1] <- rowNumbers[i]
           }
           else if(i<=cmax) {
             if(!(columnNumbers[i] %in% NA)) newColumnNumbers[length(newColumnNumbers)+1] <- columnNumbers[i]
           }
           else stop("TableCells$getCells():  argument pre-processing logic failure.", call. = FALSE)
         }
       }
       rowNumbers <- newRowNumbers
       columnNumbers <- newColumnNumbers
       cellCoordinates <- newCellCoordinates
       if((length(rowNumbers[rowNumbers %in% NA])>0)||(length(columnNumbers[columnNumbers %in% NA])>0)) {
         stop("TableCells$getCells():  Pre-processing of the row and column numbers has failed to remove the NA values.", call. = FALSE)
       }
     }
     else {
       # NA is not allowed in rowNumbers or columnNumbers
       # cells are specified as a list in the cellCoordinates parameter
       if((length(rowNumbers[rowNumbers %in% NA])>0)||(length(columnNumbers[columnNumbers %in% NA])>0)) {
         stop("TableCells$getCells():  When specifyCellsAsList=TRUE, rowNumbers/columnNumbers should not contain NA and cell coordinates should be specified using the specifyCellsAsList argument.  Please see the \"Finding and Formatting\" vignette for more details.", call. = FALSE)
       }
     }
     # check the row and column coordinates
     if(length(rowNumbers[rowNumbers > self$rowCount])>0) {
       stop("TableCells$getCells():  All rowNumbers should be less than or equal to the row count in the table.", call. = FALSE)
     }
     if(length(columnNumbers[columnNumbers > self$columnCount])>0) {
       stop("TableCells$getCells():  All columnNumbers should be less than or equal to the column count in the table.", call. = FALSE)
     }
     cellRowNumbers <- sapply(cellCoordinates, function(x) { return(x[1]) })
     cellColumnNumbers <- sapply(cellCoordinates, function(x) { return(x[2]) })
     if((length(cellRowNumbers[cellRowNumbers %in% NA])>0)||(length(cellColumnNumbers[cellColumnNumbers %in% NA])>0)) {
       stop("TableCells$getCells():  Each element in the cellCoordinates list must be a vector of length two (i.e. c(rowNumber, columnNumber)).", call. = FALSE)
     }
     if(length(cellRowNumbers[cellRowNumbers > self$rowCount])>0) {
       stop("TableCells$getCells():  All row numbers in cellCoordinates should be less than or equal to the row count in the table.", call. = FALSE)
     }
     if(length(cellColumnNumbers[cellColumnNumbers > self$columnCount])>0) {
       stop("TableCells$getCells():  All column numbers in cellCoordinates should be less than or equal to the column count in the table.", call. = FALSE)
     }
     # iterate the cells and return
     cells <- list()
     if(length(private$p_rows) > 0) {
        for(r in 1:length(private$p_rows)) {
           if(length(private$p_rows[[r]]) > 0) {
              for(c in 1:length(private$p_rows[[r]])) {
                 if(length(private$p_rows[[r]]) < c) next
                 rowMatch <- sum(r==rowNumbers) > 0
                 columnMatch <- sum(c==columnNumbers) > 0
                 cellMatch <- sum((r==cellRowNumbers)&(c==cellColumnNumbers)) > 0
                 isMatch <- FALSE
                 if(matchMode=="simple") {
                    isMatch <- rowMatch||columnMatch||cellMatch
                 }
                 else if(matchMode=="combinations") {
                    noRowCriteria <- (length(rowNumbers)==0)&&(length(rowGrpsRowNumbers)==0)
                    noColCriteria <- (length(columnNumbers)==0)&&(length(columnGrpsColumnNumbers)==0)
                    netRowMatch <- noRowCriteria||rowMatch
                    netColMatch <- noColCriteria||columnMatch
                    isMatch <- (netRowMatch&&netColMatch)||cellMatch
                 }
                 if(isMatch) {
                    cell <- private$p_rows[[r]][[c]]
                    cellIsEmpty <- is.null(cell$rawValue)
                    if(excludeEmptyCells && cellIsEmpty) next
                    cells[[length(cells)+1]] <- cell
                 }
              }
           }
        }
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getCells", "Got cells.")
     return(invisible(cells))
   },

   #' @description
   #' Find cells matching specified criteria.
   #' See the "Finding and Formatting" vignette for graphical examples.
   #' @param minValue A numerical value specifying a minimum value threshold.
   #' @param maxValue A numerical value specifying a maximum value threshold.
   #' @param exactValues A vector or list specifying a set of allowed values.
   #' @param valueRanges A vector specifying one or more value range expressions which
   #' the cell values must match.  If multiple value range expressions are specified,
   #' then the cell value must match any of one the specified expressions.
   #' @param includeNull Specify TRUE to include `NULL` in the matched cells,
   #' FALSE to exclude `NULL` values.
   #' @param includeNA Specify TRUE to include `NA` in the matched cells,
   #' FALSE to exclude `NA` values.
   #' @param emptyCells A word that specifies how empty cells are matched -
   #' must be one of "include" (default), "exclude" or "only".
   #' @param rowNumbers A vector of row numbers that specify the rows or
   #' cells to constrain the search.
   #' @param columnNumbers A vector of column numbers that specify the columns
   #' or cells to constrain the search.
   #' @param cellCoordinates A list of two-element vectors that specify the
   #' coordinates of cells to constrain the search.
   #' @param cells A `TableCell` object or a list of `TableCell`
   #' objects to constrain the scope of the search.
   #' @param rowColumnMatchMode Either "simple" (default) or "combinations":\cr
   #' "simple" specifies that row and column arguments are considered separately
   #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
   #' row 1 and all cells in column 2.\cr
   #' "combinations" specifies that row and column arguments are considered together
   #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
   #' cell single at location (1, 2).\cr
   #' Arguments `rowNumbers`, `columnNumbers`, `rowGroups` and `columnGroups` are
   #' affected by the match mode.  All other arguments are not.
   #' @return A list of `TableCell` objects.
   findCells = function(minValue=NULL, maxValue=NULL, exactValues=NULL, valueRanges=NULL, includeNull=TRUE, includeNA=TRUE, emptyCells="include",
                        rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL, cells=NULL, rowColumnMatchMode="simple") {
      if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer","numeric", "character", "logical", "date", "Date", "POSIXct", "list"), listElementsMustBeAtomic=TRUE)
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", valueRanges, missing(valueRanges), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", emptyCells, missing(emptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("TableCell", "list"), allowedListElementClasses="TableCell")
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", rowColumnMatchMode, missing(rowColumnMatchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$findCells", "Finding cells...")
      if("TableCell" %in% class(cells)) cells <- list(cells)
      cellInstanceIds <- NULL
      if((length(rowNumbers)>0)||(length(columnNumbers)>0)|(length(cellCoordinates)>0)) {
         exclEmptyCells <- FALSE
         if(emptyCells=="exclude") exclEmptyCells <- TRUE
         constrainingCells <- self$getCells(specifyCellsAsList=TRUE, excludeEmptyCells=exclEmptyCells,
                                            rowNumbers=rowNumbers, columnNumbers=columnNumbers, cellCoordinates=cellCoordinates,
                                            matchMode=rowColumnMatchMode)
         cellInstanceIds <- as.integer(sapply(constrainingCells, function(x) { x$instanceId }))
      }
      if(length(cells)>0) {
         cellInstanceIds <- union(cellInstanceIds, as.integer(sapply(cells, function(x) { x$instanceId })))
      }
      # do the searching
      matches <- list()
      if(length(private$p_rows) > 0) {
         for(r in 1:length(private$p_rows)) {
            if(length(private$p_rows[[r]]) > 0) {
               for(c in 1:length(private$p_rows[[r]])) {
                  cell <- private$p_rows[[r]][[c]]
                  # a) check missing cells
                  cellIsEmpty <- is.null(cell)
                  if((emptyCells=="exclude")&&(cellIsEmpty==TRUE)) next
                  if((emptyCells=="only")&&(cellIsEmpty==FALSE)) next
                  # b) check if one of allowed cells
                  if(length(cellInstanceIds)>0) {
                     if(!(cell$instanceId %in% cellInstanceIds)) next
                  }
                  # g) value tests:  is null, NA, minValue, maxValue, exactValues
                  if(is.null(cell$rawValue)) {
                     if(includeNull==FALSE) next
                     if((emptyCells=="exclude")&&(cellIsEmpty==TRUE)) next
                     if((emptyCells=="only")&&(cellIsEmpty==FALSE)) next
                  }
                  else if(length(cell$rawValue)==0) {
                     if(includeNull==FALSE) next
                     if((emptyCells=="exclude")&&(cellIsEmpty==TRUE)) next
                     if((emptyCells=="only")&&(cellIsEmpty==FALSE)) next
                  }
                  else {
                     if(is.na(cell$rawValue)) {
                        if(includeNA==FALSE) next
                     }
                     else {
                        if((!is.null(minValue))||(!is.null(maxValue))) {
                           cls <- class(cell$rawValue)
                           if(("integer" %in% cls)||("numeric" %in% cls)) {
                              if(!is.null(minValue)) {
                                 if(cell$rawValue < minValue) next
                              }
                              if(!is.null(maxValue)) {
                                 if(cell$rawValue > maxValue) next
                              }
                           }
                           else next
                        }
                        if(!is.null(exactValues)) {
                           if(!(cell$rawValue %in% exactValues)) next
                        }
                     }
                  }
                  # h) value range expressions
                  if(length(valueRanges)>0) {
                     vreMatch <- FALSE
                     for (vre in valueRanges) {
                        if(vreIsMatch(vre, cell$rawValue)) {
                           vreMatch <- TRUE
                           break
                        }
                     }
                     if(!vreMatch) next
                  }
                  # is a match
                  matches[[length(matches)+1]] <- cell
               }
            }
         }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$findCells", "Found cells.")
      return(invisible(matches))
   },

   #' @description
   #' Retrieve the width of the longest value
   #'   (in characters) in each column.
    #' @return The width of the column in characters.
   getColumnWidths = function() {
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getColumnWidths", "Getting column widths...")
     widths <- integer(0)
     if((self$rowCount>0)&&(self$columnCount>0)) {
       widths <- integer(self$columnCount)
       for(r in 1:self$rowCount) {
         if(length(private$p_rows[[r]])==0) next
         for(c in 1:length(private$p_rows[[r]])) {
           cell <- private$p_rows[[r]][[c]]
           if(is.null(cell)) next
           if(is.null(cell$formattedValue)) next
           if(is.na(cell$formattedValue)) next
           widths[c] <- max(widths[c], nchar(cell$formattedValue))
         }
       }
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getColumnWidths", "Got column widths.")
     return(invisible(widths))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         rlst <- list()
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             if(is.null(private$p_rows[[r]][[c]])) next
             rlst[[c]] <- private$p_rows[[r]][[c]]$asList()
           }
         }
         lst[[r]] <- rlst
       }
     }
     return(invisible(lst))
   },

   #' @description
   #' Return the contents of this object as JSON for debugging.
   #' @return A JSON representation of various object properties.
   asJSON = function() {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
         stop("The jsonlite package is needed to convert to JSON.  Please install the jsonlite package.", call. = FALSE)
      }
      jsonliteversion <- utils::packageDescription("jsonlite")$Version
      if(numeric_version(jsonliteversion) < numeric_version("1.1")) {
         stop("Version 1.1 or above of the jsonlite package is needed to convert to JSON.  Please install an updated version of the jsonlite package.", call. = FALSE)
      }
      return(jsonlite::toJSON(self$asList()))
   }
  ),
  active = list(

   #' @field rowCount The number of rows in the table.
   rowCount = function(value) { return(invisible(length(private$p_rows))) },

   #' @field columnCount The number of columns in the table.
   columnCount = function(value) { return(invisible(private$p_columnCount)) },

   #' @field rows The rows of cells in the table - represented as a list, each
   #'   element of which is a list of `TableCell` objects.
   rows = function(value) { return(invisible(private$p_rows)) },

   #' @field all A list of the cells in the table.  Each element in this list is
   #' a `TableCell` object.
   all = function(value) { return(self$getCells(rowNumbers=1:self$rowCount)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_rows = NULL,
    p_columnCount = NULL
  )
)
