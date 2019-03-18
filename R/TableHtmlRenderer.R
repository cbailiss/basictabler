#' A class that renders a table in HTML.
#'
#' The TableHtmlRenderer class creates a HTML representation of a table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmltools
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   render to HTML.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should not be used by end users.  It is an internal class
#' # created only by the BasicTable class.  It is used when rendering to HTML.
#' # See the package vignettes for more information about outputs.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' tbl$renderTable()
#' @field parentTable Owning table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new table renderer, specifying the
#'   field value documented above.}
#'
#'   \item{\code{getTableHtml(styleNamePrefix=NULL)}}{Get a HTML representation
#'   of the table.}
#' }

TableHtmlRenderer <- R6::R6Class("TableHtmlRenderer",
  public = list(
   initialize = function(parentTable) {
     if(parentTable$argumentCheckMode > 0) {
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableHtmlRenderer", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableHtmlRenderer$new", "Creating new Html Renderer...")
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableHtmlRenderer$new", "Created new Html Renderer.")
   },
   getTableHtml = function(styleNamePrefix=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableHtmlRenderer", "getTableHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableHtmlRenderer$getTableHtml", "Getting table HTML...")
     # get the style names
     styles <- names(private$p_parentTable$styles$styles)
     defaultTableStyle = private$p_parentTable$styles$tableStyle
     defaultRootStyle = private$p_parentTable$styles$rootStyle
     defaultRowHeaderStyle = private$p_parentTable$styles$rowHeaderStyle
     defaultColHeaderStyle = private$p_parentTable$styles$colHeaderStyle
     defaultCellStyle = private$p_parentTable$styles$cellStyle
     defaultTotalStyle = private$p_parentTable$styles$totalStyle
     # get the actual style names to use, including the styleNamePrefix
     tableStyle <- paste0(styleNamePrefix, defaultTableStyle)
     rootStyle <- paste0(styleNamePrefix, defaultRootStyle)
     rowHeaderStyle <- paste0(styleNamePrefix, defaultRowHeaderStyle)
     colHeaderStyle <- paste0(styleNamePrefix, defaultColHeaderStyle)
     cellStyle <- paste0(styleNamePrefix, defaultCellStyle)
     totalStyle <- paste0(styleNamePrefix, defaultTotalStyle)
     # get the dimensions of the table...
     rowCount <- private$p_parentTable$cells$rowCount
     columnCount <- private$p_parentTable$cells$columnCount
     # special case of no rows and no columns, return a blank empty table
     if((rowCount==0)&&(columnCount==0)) {
       tbl <- htmltools::tags$table(class=tableStyle, htmltools::tags$tr(
         htmltools::tags$td(class=cellStyle, style="text-align: center; padding: 6px", htmltools::HTML("(no data)"))))
       return(tbl)
     }
     # update the merged cell info
     private$p_parentTable$applyCellMerges()
     # build the table up row by row
     trows <- list()
     # render the rows
     for(r in 1:rowCount) {
       trow <- list()
       # render the cell values
       for(c in 1:columnCount) {
         # get the cell
         cell <- private$p_parentTable$cells$getCell(r, c)
         # if a merged cell and not the root of the merge, then skip to next cell
         if(cell$isMerged && (!cell$isMergeRoot)) { next }
         # get the styling info
         if(cell$cellType=="root") cssCell <- rootStyle
         else if(cell$cellType=="rowHeader") cssCell <- rowHeaderStyle
         else if(cell$cellType=="columnHeader") cssCell <- colHeaderStyle
         else if(cell$cellType=="cell") cssCell <- cellStyle
         else if(cell$cellType=="total") cssCell <- totalStyle
         if(!is.null(cell$baseStyleName)) cssCell <- paste0(styleNamePrefix, cell$baseStyleName)
         cllstyl <- NULL
         if(!is.null(cell$style)) cllstyl <- cell$style$asCSSRule()
         # output the cell
         renderAsTH <- (cell$cellType=="root") || (cell$cellType=="rowHeader") || (cell$cellType=="columnHeader")
         renderAsTH <- renderAsTH && (!isTRUE(private$p_parentTable$compatibility$headerCellsAsTD))
         if(renderAsTH)
         {
           # th cells
           if(cell$isMerged) {
             mergeRange <- private$p_parentTable$mergedCells$ranges[[cell$mergeIndex]]
             if(cell$visible) trow[[length(trow)+1]] <- htmltools::tags$th(rowspan=mergeRange$rSpan, colspan=mergeRange$cSpan,
                                                                           class=cssCell, style=cllstyl, cell$formattedValue)
             else trow[[length(trow)+1]] <- htmltools::tags$th(rowspan=mergeRange$rSpan, colspan=mergeRange$cSpan,
                                                               class=cssCell, style=cllstyl) # todo: check escaping
           }
           else {
             if(cell$visible) trow[[length(trow)+1]] <- htmltools::tags$th(class=cssCell, style=cllstyl, cell$formattedValue)
             else trow[[length(trow)+1]] <- htmltools::tags$th(class=cssCell, style=cllstyl) # todo: check escaping
           }
         }
         else {
           # td cells
           if(cell$isMerged) {
             mergeRange <- private$p_parentTable$mergedCells$ranges[[cell$mergeIndex]]
             if(cell$visible) trow[[length(trow)+1]] <- htmltools::tags$td(rowspan=mergeRange$rSpan, colspan=mergeRange$cSpan,
                                                                           class=cssCell, style=cllstyl, cell$formattedValue)
             else trow[[length(trow)+1]] <- htmltools::tags$td(rowspan=mergeRange$rSpan, colspan=mergeRange$cSpan,
                                                               class=cssCell, style=cllstyl) # todo: check escaping
           }
           else {
             if(cell$visible) trow[[length(trow)+1]] <- htmltools::tags$td(class=cssCell, style=cllstyl, cell$formattedValue)
             else trow[[length(trow)+1]] <- htmltools::tags$td(class=cssCell, style=cllstyl) # todo: check escaping
           }
         }
       }
       # finished this row
       trows[[length(trows)+1]] <- htmltools::tags$tr(trow)
     }
     tbl <- htmltools::tags$table(class=tableStyle, trows)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableHtmlRenderer$getTableHtml", "Got table HTML.")
     return(invisible(tbl))
   }
  ),
  private = list(
    p_parentTable = NULL
  )
)
