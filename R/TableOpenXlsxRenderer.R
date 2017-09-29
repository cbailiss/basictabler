#' A class that renders a table into an Excel worksheet.
#'
#' The TableOpenXlsxRenderer class creates a representation of a table in an Excel file using the openxlsx package.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmltools
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   render to Excel via the openxlsx package
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should not be used by end users.  It is an internal class
#' # created only by the BasicTable class.  It is used when rendering to Excel.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' library(openxlsx)
#' wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#' addWorksheet(wb, "Data")
#' tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#'                          topRowNumber=1, leftMostColumnNumber=1,
#'                          applyStyles=TRUE, mapStylesFromCSS=TRUE)
#' # Use saveWorkbook() to save the Excel file.
#' @field parentTable Owning table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new table renderer, specifying the
#'   field value documented above.}
#'
#'   \item{\code{writeToCell(wb=NULL, wsName=NULL, rowNumber=NULL,
#'   columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL,
#'   style=NULL, mapFromCss=TRUE)}}{Writes a value to a cell and applies styling
#'   as needed.}
#'   \item{\code{writeToWorksheet(wb=NULL, wsName=NULL, topRowNumber=NULL,
#'   leftMostColumnNumber=NULL, outputValuesAs="value", applyStyles=TRUE,
#'   mapStylesFromCSS=TRUE)}}{Output the table into the specified workbook
#'   and worksheet at the specified row-column location.}
#' }

TableOpenXlsxRenderer <- R6::R6Class("TableOpenXlsxRenderer",
  public = list(
    initialize = function(parentTable) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$new", "Creating new OpenXlsx Renderer...")
      private$p_styles <- TableOpenXlsxStyles$new(parentTable=private$p_parentTable)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$new", "Created new OpenXlsx Renderer.")
    },
    writeToCell = function(wb=NULL, wsName=NULL, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL) {
       if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "character", "Date", "POSIXct", "POSIXlt"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", mergeRows, missing(mergeRows), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToCell", mergeColumns, missing(mergeColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$writeToWorksheet", "Writing to cell...")
      # write the value
      openxlsx::writeData(wb, sheet=wsName, x=value, colNames=FALSE, rowNames=FALSE, startCol=columnNumber, startRow=rowNumber)
      # merge cells
      isMergedCells <- isNumericValue(mergeRows)&&isNumericValue(mergeColumns)
      if(isMergedCells) openxlsx::mergeCells(wb, sheet=wsName, cols=mergeColumns, rows=mergeRows)
      # styling
      if(applyStyles) {
        openxlsxStyle <- NULL
        # just a base style (these were all already added to the OpenXlsxStyles collection, so can just do a find based on the name only)
        if(isTextValue(baseStyleName)&&is.null(style)) {
          openxlsxStyle <- private$p_styles$findNamedStyle(baseStyleName)
          if(is.null(openxlsxStyle)) stop(paste0("TableOpenXlsxRenderer$writeToWorksheet(): Unable to find named style '", baseStyleName, "'."), call. = FALSE)
          if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
        }
        # base style and overlaid style, or just an overlaid style
        else if(!is.null(style)) {
          if(isTextValue(baseStyleName)) {
            # need to get the base style and overlay the additional style attributes
            baseStyle <- private$p_parentTable$styles$getStyle(baseStyleName)
            fullStyle <- baseStyle$getCopy(newStyleName="")
            fullStyle$setPropertyValues(style$declarations)
          }
          else fullStyle <- style
          openxlsxStyle <- private$p_styles$findOrAddStyle(action="findOrAdd", baseStyleName=baseStyleName, isBaseStyle=FALSE, style=fullStyle, mapFromCss=mapFromCss)
          if(is.null(openxlsxStyle)) stop("TableOpenXlsxRenderer$writeToWorksheet(): Failed to find or add style.", call. = FALSE)
          if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
        }
        # min heights/widths
        if(!is.null(openxlsxStyle)) {
          cw <- openxlsxStyle$minColumnWidth
          rh <- openxlsxStyle$minRowHeight
          if((!is.null(cw)) && (cw > private$p_minimumColumnWidths[columnNumber])) private$p_minimumColumnWidths[columnNumber] <- cw
          if((!is.null(rh)) && (rh > private$p_minimumRowHeights[rowNumber])) private$p_minimumRowHeights[rowNumber] <- rh
        }
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$writeToWorksheet", "Written to cell.")
    },
    writeToWorksheet = function(wb=NULL, wsName=NULL, topRowNumber=NULL, leftMostColumnNumber=NULL, outputValuesAs="rawValue", applyStyles=TRUE, mapStylesFromCSS=TRUE) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", topRowNumber, missing(topRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", leftMostColumnNumber, missing(leftMostColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", outputValuesAs, missing(outputValuesAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableOpenXlsxRenderer", "writeToWorksheet", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$writeToWorksheet", "Writing to worksheet...")

      # clear the styles
      private$p_styles$clearStyles()

      # create an OpenXlsxStyle for each named style in the table
      if(applyStyles) private$p_styles$addNamedStyles(mapFromCss=mapStylesFromCSS)

      # output the styles
      # message(private$p_styles$asString(seperator="\r\n\r\n"))

      # do the export, going cell by cell
      # for each header and cell, if the style is basic, find the basic style matching on name only
      # if the style is not basic (i.e. has additional style settings applied directly to that header/cell)
      # then need to do a full match on all of the details of the style (slower)

      # get the style names
      tableStyle = private$p_parentTable$styles$tableStyle
      rootStyle = private$p_parentTable$styles$rootStyle
      rowHeaderStyle = private$p_parentTable$styles$rowHeaderStyle
      colHeaderStyle = private$p_parentTable$styles$colHeaderStyle
      cellStyle = private$p_parentTable$styles$cellStyle
      totalStyle = private$p_parentTable$styles$totalStyle

      # ...cells:
      rowCount <- private$p_parentTable$cells$rowCount
      columnCount <- private$p_parentTable$cells$columnCount

      # initialise the minimum widths/heights
      private$p_minimumRowHeights = numeric(1048576)
      private$p_minimumColumnWidths = numeric(16384)

      # special case of no rows and no columns, return a blank empty table
      if((rowCount==0)&&(columnCount==0)) {
        self$writeToCell(wb, wsName, rowNumber=topRowNumber, columnNumber=leftMostColumnNumber, value="(no data)",
                         applyStyles=applyStyles, baseStyleName=cellStyle, style=NULL, mapFromCss=mapStylesFromCSS)
        return(invisible(NULL))
      }

      # render the rows
      for(r in 1:rowCount) {
        # row number
        xlRowNumber <- topRowNumber + r - 1
        xlColumnNumber <- leftMostColumnNumber
        # render the cell values
        for(c in 1:columnCount) {
          xlRowNumber <- topRowNumber + r - 1
          xlColumnNumber <- leftMostColumnNumber + c - 1
          cell <- private$p_parentTable$cells$getCell(r, c)
          if(cell$cellType=="root") cs <- rootStyle
          else if(cell$cellType=="rowHeader") cs <- rowHeaderStyle
          else if(cell$cellType=="columnHeader") cs <- colHeaderStyle
          else if(cell$cellType=="total") cs <- totalStyle
          else cs <- cellStyle
          if(!is.null(cell$baseStyleName)) cs <- cell$baseStyleName
          if(outputValuesAs=="rawValue") value <- cell$rawValue
          else if(outputValuesAs=="formattedValueAsText") value <- cell$formattedValue
          else if(outputValuesAs=="formattedValueAsNumber") {
            value <- suppressWarnings(as.numeric(cell$formattedValue))
            if(!isNumericValue(value)) value <- cell$formattedValue
          }
          else value <- cell$rawValue
          if(is.factor(value)) value <- as.character(value)
          self$writeToCell(wb, wsName, rowNumber=xlRowNumber, columnNumber=xlColumnNumber, value=value,
                           applyStyles=applyStyles, baseStyleName=cs, style=cell$style, mapFromCss=mapStylesFromCSS)
        }
      }

      # set the minimum heights / widths
      for(r in 1:length(private$p_minimumRowHeights)) {
        if(private$p_minimumRowHeights[r] > 0)
          openxlsx::setRowHeights(wb, sheet=wsName, rows=r, heights=private$p_minimumRowHeights[r])
      }
      for(c in 1:length(private$p_minimumColumnWidths)) {
        if(private$p_minimumColumnWidths[c] > 0)
          openxlsx::setColWidths(wb, sheet=wsName, cols=c, widths=private$p_minimumColumnWidths[c])
      }

      # TRELLO NOTES

      # TableOpenXlsxStyles collection builds up a collection of styles defined
      # in the Excel document.  These can be reused as follows: Before rendering
      # begins, a TableOpenXlsxStyle object is created for each named style
      # defined in the table. As each cell is rendered to the workbook, if
      # a cell with a named style but no other style overrides is
      # encountered, then these styles are used.  If a cell is encountered
      # with overrides, then the styles collection is searched to find the
      # matching style.  If not found, then a new style is added which can be
      # reused later. The TableOpenXlsxStyle object looks for styles named
      # "xl-".  If found, these are used.  These closely map to the arguments of
      # the openxlsx createStyle() function.  If style settings named "xl-" are
      # not found, then an attempt is made to use the CSS equivalents.  See the
      # ExcelExport.xlsx file for details. Currently, the following still needs
      # doing in terms of style settings:
      #
      # * font-name, font-size, etc have been mapped, but the general CSS font
      # setting (e.g. font: 15px arial, sans-serif;) has not been mapped.
      # * border settings have been partially mapped.  border, border-top,
      # border-bottom, etc have been mapped.  The very specific (e.g.
      # border-bottom-width, border-bottom-style, etc) have not been mapped.

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxRenderer$writeToWorksheet", "Written to worksheet.")
    }
  ),
  private = list(
    p_parentTable = NULL,
    p_styles = NULL,
    p_minimumRowHeights = NULL,
    p_minimumColumnWidths = NULL
  )
)
