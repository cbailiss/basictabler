#' A class that contains the cells from a table.
#'
#' The TableCells class contains all of the TableCell objects that comprise a
#' table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods relating
#'   to the cells of a table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the table.
#' # It is not intended to be created outside of the table.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' cells <- tbl$cells
#' cells$setCell(r=4, c=1, cellType="cell", rawValue=5)
#' cells$setCell(r=4, c=2, cellType="cell", rawValue=6)
#' tbl$renderTable()
#' @field parentTable Owning table.
#' @field rows The rows of cells in the table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new set of table cells, specifying
#'   the field values documented above.}
#'
#'   \item{\code{reset())}}{Clears and removes all of the cells.}
#'   \item{\code{getCell(r, c))}}{Get the TableCell at the specified row and
#'   column coordinates in the table.}
#'   \item{\code{getValue(r, c))}}{Get the value at the specified row and
#'   column coordinates in the table.}
#'   \item{\code{getRowValues(rowNumber=NULL, columnNumbers=NULL,
#'   formattedValue=FALSE, asList=FALSE, rebase=TRUE)}}{Get a vector or list of
#'   the values in a row.}
#'   \item{\code{getColumnValues(columnNumber=NULL, rowNumbers=NULL,
#'   formattedValue=FALSE, asList=FALSE, rebase=TRUE)}}{Get a vector or list of
#'   the values in a column.}
#'   \item{\code{setCell(r, c, cellType="cell", rawValue=NULL,
#'   formattedValue=NULL, visible=TRUE, baseStyleName=NULL,
#'   styleDeclarations=NULL)}}{Set the details of a cell in the table.}
#'   \item{\code{setBlankCell(r, c, cellType="cell", visible=TRUE,
#'   baseStyleName=NULL, styleDeclarations=NULL)}}{Set a cell to be empty in the
#'   table.}
#'   \item{\code{deleteCell(r, c)}}{Remove a cell from the table (replacing it
#'   with a blank one).}
#'   \item{\code{setValue(r, c, rawValue=NULL, formattedValue=NULL)}}{Set the
#'   value of a cell.}
#'   \item{\code{extendCells(rowCount=NULL, columnCount=NULL)}}{.}
#'   \item{\code{moveCell(r, c, cell))}}{Move the cell to the specified row
#'   and column coordinates in the table.}
#'   \item{\code{insertRow(rowNumber=NULL)}}{Insert a new row (moving the rows
#'   underneath down).}
#'   \item{\code{setRow(rowNumber=NULL, startAtColumnNumber=1, cellTypes=NULL,
#'   rawValues=NULL, formattedValues=NULL, formats=NULL, visiblity=TRUE,
#'   baseStyleNames=NULL)}}{Set multiple cells across a row at once.}
#'   \item{\code{setColumn(columnNumber=NULL, startAtRowNumber=1,
#'   cellTypes=NULL, rawValues=NULL, formattedValues=NULL, formats=NULL,
#'   visiblity=TRUE, baseStyleNames=NULL)}}{Set multiple cells down a column at
#'   once.}
#'   \item{\code{deleteRow(rowNumber=NULL)}}{Delete a row (moving the rows
#'   underneath up.}
#'   \item{\code{insertColumn(columnNumber=NULL)}}{Insert a new column (moving
#'   other columns rightwards.}
#'   \item{\code{deleteColumn(columnNumber=NULL)}}{Delete a column (moving other
#'   columns leftwards.}
#'   \item{\code{getCells(specifyCellsAsList=FALSE, rowNumbers=NULL,
#'   columnNumbers=NULL, cellCoordinates=NULL)}}{Retrieve cells by a combination
#'   of row and/or column numbers.}
#'   \item{\code{findCells(rowNumbers=NULL, columnNumbers=NULL, minValue=NULL,
#'   maxValue=NULL, exactValues=NULL, includeNull=TRUE, includeNA=TRUE)}}{Find
#'   cells matching the specified criteria.}
#'   \item{\code{getColumnWidths())}}{Retrieve the width of the longest value
#'   (in characters) in each column.}
#'   \item{\code{asList())}}{Get a list representation of the table
#'   cells.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the table cells.}
#' }

TableCells <- R6::R6Class("TableCells",
  public = list(
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
   reset = function() {
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$resetCells", "Resetting cells...")
     private$p_rows <- NULL
     private$p_columnCount <- 0
     private$p_parentTable$mergedCells$clear()
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$resetCells", "Reset cells.")
   },
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
   setCell = function(r=NULL, c=NULL, cellType="cell", rawValue=NULL, formattedValue=NULL, visible=TRUE, baseStyleName=NULL, styleDeclarations=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", cellType, missing(cellType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", visible, missing(visible), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", styleDeclarations, missing(styleDeclarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     if(missing(formattedValue)) formattedValue <- rawValue
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, rawValue=rawValue, formattedValue=formattedValue, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations)
     self$moveCell(r, c, cell)
     return(invisible(cell))
   },
   setBlankCell = function(r=NULL, c=NULL, cellType="cell", visible=TRUE, baseStyleName=NULL, styleDeclarations=NULL) {
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, visible=visible, rawValue=NULL, formattedValue=NULL, baseStyleName=baseStyleName, styleDeclarations=styleDeclarations)
     self$moveCell(r, c, cell)
     return(invisible(cell))
   },
   deleteCell = function(r=NULL, c=NULL) {
     cell <- self$setBlankCell(r, c, visible=FALSE)
     return(invisible(cell))
   },
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
   setRow = function(rowNumber=NULL, startAtColumnNumber=1, cellTypes="cell", rawValues=NULL, formattedValues=NULL, formats=NULL, visiblity=TRUE, baseStyleNames=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", startAtColumnNumber, missing(startAtColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", cellTypes, missing(cellTypes), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", rawValues, missing(rawValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", formattedValues, missing(formattedValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", formats, missing(formats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", visiblity, missing(visiblity), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setRow", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
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
       if(length(formattedValues)==0) {
         if(!is.null(format)) formattedValue <- private$p_parentTable$formatValue(value=v, format=format)
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
   setColumn = function(columnNumber=NULL, startAtRowNumber=2, cellTypes="cell", rawValues=NULL, formattedValues=NULL, formats=NULL, visiblity=TRUE, baseStyleNames=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", startAtRowNumber, missing(startAtRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", cellTypes, missing(cellTypes), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("root", "rowHeader", "columnHeader", "cell", "total"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", rawValues, missing(rawValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", formattedValues, missing(formattedValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"), allowedListElementClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", formats, missing(formats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", visiblity, missing(visiblity), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setColumn", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
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
       if(length(formattedValues)==0) {
         if(!is.null(format)) formattedValue <- private$p_parentTable$formatValue(value=v, format=format)
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
   insertRow = function(rowNumber=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertRow", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
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
     for(c in 1:self$columnCount) {
       self$setBlankCell(rowNumber, c)
     }
     private$p_parentTable$mergedCells$updateAfterRowInsert(rowNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertRow", "Inserted row.")
     return(invisible())
   },
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
   insertColumn = function(columnNumber=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "insertColumn", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
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
     for(r in 1:self$rowCount) {
       self$setBlankCell(r, columnNumber)
     }
     private$p_parentTable$mergedCells$updateAfterColumnInsert(columnNumber)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$insertColumn", "Inserted column")
     return(invisible())
   },
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
   getCells = function(specifyCellsAsList=TRUE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
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
     # if no rows, columns or cells specified, then return all cells
     cells <- list()
     if(is.null(rowNumbers)&&is.null(columnNumbers)&&(length(cellCoordinates)==0)) {
       if(length(private$p_rows) > 0) {
         for(r in 1:length(private$p_rows)) {
           if(length(private$p_rows[[r]]) > 0) {
             for(c in 1:length(private$p_rows[[r]])) {
               if(length(private$p_rows[[r]]) < c) next
               cell <- private$p_rows[[r]][[c]]
               cells[[length(cells)+1]] <- cell
             }
           }
         }
       }
       if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getCells", "Got cells.")
       return(invisible(cells))
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
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             if(length(private$p_rows[[r]]) < c) next
             rowMatch <- sum(r==rowNumbers) > 0
             columnMatch <- sum(c==columnNumbers) > 0
             cellMatch <- sum((r==cellRowNumbers)&(c==cellColumnNumbers)) > 0
             if(rowMatch||columnMatch||cellMatch) {
               cell <- private$p_rows[[r]][[c]]
               cells[[length(cells)+1]] <- cell
             }
           }
         }
       }
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$getCells", "Got cells.")
     return(invisible(cells))
   },
   findCells = function(rowNumbers=NULL, columnNumbers=NULL,
                        minValue=NULL, maxValue=NULL, exactValues=NULL, includeNull=TRUE, includeNA=TRUE) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$findCells", "Finding cells...")
     matches <- list()
     if(length(private$p_rows) > 0) {
       for(r in 1:length(private$p_rows)) {
         # a) row number tests
         if(!is.null(rowNumbers)) {
           if(!(r %in% rowNumbers)) next
         }
         if(length(private$p_rows[[r]]) > 0) {
           for(c in 1:length(private$p_rows[[r]])) {
             # b) column number tests
             if(!is.null(columnNumbers)) {
               if(!(c %in% columnNumbers)) next
             }
             cell <- private$p_rows[[r]][[c]]
             if(is.null(cell)) next
             rowColFilters <- cell$rowColFilters
             # c) value tests:  is null, NA, minValue, maxValue, exactValues
             if(is.null(cell$rawValue)) {
               if(includeNull==FALSE) next
             }
             else if(length(cell$rawValue)==0) {
               if(includeNull==FALSE) next
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
             # is a match
             matches[[length(matches)+1]] <- cell
           }
         }
       }
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$findCells", "Found cells.")
     return(invisible(matches))
   },
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
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowCount = function(value) { return(invisible(length(private$p_rows))) },
   columnCount = function(value) { return(invisible(private$p_columnCount)) },
   rows = function(value) { return(invisible(private$p_rows)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_rows = NULL,
    p_columnCount = NULL
  )
)
