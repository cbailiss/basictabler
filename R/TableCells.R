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
#'   \item{\code{addCell(r, c, cellType="cell", rawValue=NULL,
#'   formattedValue=NULL)}}{Add a new cell to the table.}
#'   \item{\code{addBlankCell(r, c, cellType="cell", visible=TRUE)}}{Add an
#'   empty cell to the table.}
#'   \item{\code{deleteCell(r, c)}}{Remove a cell from the table (replacing it
#'   with a blank one).}
#'   \item{\code{setValue(r, c, rawValue=NULL, formattedValue=NULL)}}{Set the
#'   value of a cell.}
#'   \item{\code{extendCells(rowCount=NULL, columnCount=NULL)}}{.}
#'   \item{\code{setCell(r, c, cell))}}{Set the TableCell at the specified row
#'   and column coordinates in the table.}
#'   \item{\code{insertRow(rowNumber=NULL)}}{Insert a new row (moving the rows
#'   underneath down).}
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
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCells$resetCells", "Reset cells.")
   },
   getCell = function(r=NULL, c=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$rowCount)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "getCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1, maxValue=self$columnCount)
     }
     if(r < 1)
       stop(paste0("TableCells$getCell(): r (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(r > self$rowCount)
       stop(paste0("TableCells$getCell(): r (", r, ") must be less than or equal to rowCount (", self$rowCount, ")."), call. = FALSE)
     if(c < 1)
       stop(paste0("TableCells$getCell(): c (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(c > self$columnCount)
       stop(paste0("TableCells$getCell(): c (", c, ") must be less than or equal to columnCount (", self$columnCount, ")."), call. = FALSE)
     if(length(private$p_rows[[r]]) < c) return(invisible(NULL))
     return(invisible(private$p_rows[[r]][[c]]))
   },
   addCell = function(r, c, cellType="cell", rawValue=NULL, formattedValue=NULL) {
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, rawValue=rawValue, formattedValue=formattedValue)
     self$setCell(r, c, cell)
     return(invisible(cell))
   },
   addBlankCell = function(r, c, cellType="cell", visible=TRUE) {
     cell <- TableCell$new(parentTable=private$p_parentTable, rowNumber=r, columnNumber=c, cellType=cellType, visible=visible, rawValue=NULL, formattedValue=NULL)
     self$setCell(r, c, cell)
     return(invisible(cell))
   },
   deleteCell = function(r, c) {
     cell <- self$addBlankCell(r, c, visible=FALSE)
     return(invisible(cell))
   },
   setValue = function(r, c, rawValue=NULL, formattedValue=NULL) {
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
   extendCells = function(rowCount=NULL, columnCount=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", rowCount, missing(rowCount), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", columnCount, missing(columnCount), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
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
   setCell = function(r, c, cell) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"), minValue=1)
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCells", "setCell", cell, missing(cell), allowMissing=FALSE, allowNull=FALSE, allowedClasses="TableCell")
     }
     if(r < 1)
       stop(paste0("TableCells$setCell(): r (", r, ") must be must be greater than or equal to 1."), call. = FALSE)
     if(c < 1)
       stop(paste0("TableCells$setCell(): c (", c, ") must be must be greater than or equal to 1."), call. = FALSE)
     if((r > self$rowCount)||(c > self$columnCount)) self$extendCells(r, c)
     private$p_rows[[r]][[c]] <- cell
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
           cell$updatePosition(r + 1, c)
           self$setCell(r + 1, c, cell)
         }
       }
     }
     for(c in 1:self$columnCount) {
       self$addBlankCell(rowNumber, c)
     }
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
           cell$updatePosition(r, c)
           self$setCell(r, c, cell)
         }
       }
     }
     private$p_rows[[self$rowCount]] <- NULL # can set last cell in the list to NULL to shorten the array
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
           cell$updatePosition(r, c + 1)
           self$setCell(r, c + 1, cell)
         }
       }
     }
     for(r in 1:self$rowCount) {
       self$addBlankCell(r, columnNumber)
     }
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
           cell$updatePosition(r, c)
           self$setCell(r, c, cell)
         }
       }
     }
     for(r in 1:self$rowCount) {
       private$p_rows[[r]][[self$columnCount]] <- NULL # can set last cell in the list to NULL to shorten the array
     }
     private$p_columnCount <- private$p_columnCount - 1
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
