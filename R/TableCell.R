#' A class that represents a cell in a table
#'
#' The TableCell class represents a cell in a table.  Both header cells and body
#' cells are represented by this class.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a single table cell
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by the table.
#' # It is not intended to be created outside of the table.
#' @field parentTable Owning table.
#' @field rowNumber The row number of the cell.  1 = the first (i.e. top) data
#'   row.
#' @field columnNumber The column number of the cell.  1 = the first (i.e.
#'   leftmost) data column.
#' @field rawValue The original unformatted value.
#' @field formattedValue The formatted value (i.e. normally of character data
#'   type).
#' @field baseStyleName The name of the style applied to this cell (a character
#'   value).  The style must exist in the TableStyles object associated with the
#'   table.
#' @field style A TableStyle object that can apply overrides to the base style
#'   for this cell.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new table cell, specifying the field
#'   values documented above.}
#'
#'   \item{\code{getCopy())}}{Get a copy of this cell.}
#'   \item{\code{asList())}}{Get a list representation of this cell}
#'   \item{\code{asJSON()}}{Get a JSON representation of this cell}
#' }

TableCell <- R6::R6Class("TableCell",
  public = list(
   initialize = function(parentTable, rowNumber=NULL, columnNumber=NULL, rawValue=NULL, formattedValue=NULL) {
     if(parentTable$argumentCheckMode > 0) {
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", rowNumber, missing(rowNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", columnNumber, missing(columnNumber), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "Date", "POSIXct", "POSIXlt"))
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableCell", "initialize", formattedValue, missing(formattedValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "Date", "POSIXct", "POSIXlt"))
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCell$new", "Creating new TableCell",
                                   list(rowNumber=rowNumber, columnNumber=columnNumber))
     private$p_rowNumber <- rowNumber
     private$p_columnNumber <- columnNumber
     private$p_rawValue <- rawValue
     private$p_formattedValue <- formattedValue
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableCell$new", "Created new TableCell")
   },
   getCopy = function() {
     copy <- list()
     return(invisible(copy))
   },
   asList = function() {
     lst <- list(
       row=private$p_rowNumber,
       column=private$p_columnNumber,
       rawValue=private$p_rawValue,
       formattedValue=private$p_formattedValue
     )
     return(invisible(lst))
   },
   asJSON = function() { return(jsonlite::toJSON(asList())) }
  ),
  active = list(
   rowNumber = function(value) { return(invisible(private$p_rowNumber)) },
   columnNumber = function(value) { return(invisible(private$p_columnNumber)) },
   rawValue = function(value) {
     if(missing(value)) return(invisible(private$p_rawValue))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "rawValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "Date", "POSIXct", "POSIXlt"))
       }
       private$p_rawValue <- value
       return(invisible())
     }
   },
   formattedValue = function(value) {
     if(missing(value)) return(invisible(private$p_formattedValue))
     else {
       if(private$p_parentTable$argumentCheckMode > 0) {
         checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableCell", "formattedValue", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "Date", "POSIXct", "POSIXlt"))
       }
       private$p_formattedValue <- value
       return(invisible())
     }
   },
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
    p_rawValue = NULL ,               # a value (unique to this cell)
    p_formattedValue = NULL,          # a value (unique to this cell)
    p_baseStyleName = NULL,           # a string
    p_style = NULL                    # an object ref (may or may not be shared) to a TableStyle object
  )
)
