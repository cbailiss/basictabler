#' A class that defines a basic table.
#'
#' The BasicTable class represents a table with styling and formatting that can
#' be rendered to multiple output formats.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmlwidgets
#' @import htmltools
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define a basic table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # The package vignettes have many more examples of working with the
#' # BasicTable class.
#' # Quickly rendering a table as an htmlwidget:
#' library(basictabler)
#' qhtbl(data.frame(a=1:2, b=3:4))
#' # Rendering a larger table as an htmlwidget:
#' library(basictabler)
#' library(dplyr)
#' tocsummary <- bhmsummary %>%
#'   group_by(TOC) %>%
#'   summarise(OnTimeArrivals=sum(OnTimeArrivals),
#'             OnTimeDepartures=sum(OnTimeDepartures),
#'             TotalTrains=sum(TrainCount)) %>%
#'   ungroup() %>%
#'   mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
#'          OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
#'   arrange(TOC)
#'
#' tbl <- BasicTable$new()
#' columnHeaders <- c("TOC", "On-Time Arrivals", "On-Time Departures",
#'   "Total Trains", "On-Time Arrival %", "On-Time Departure %")
#' columnFormats=list()
#' columnFormats[[2]] <- list(big.mark=",")
#' columnFormats[[3]] <- list(big.mark=",")
#' columnFormats[[4]] <- list(big.mark=",")
#' columnFormats[[5]] <- "%.1f"
#' columnFormats[[6]] <- "%.1f"
#' tbl$addData(tocsummary, columnNamesAsColumnHeaders=FALSE,
#'   firstColumnAsRowHeaders=TRUE,
#'   explicitColumnHeaders=columnHeaders, columnFormats=columnFormats)
#' tbl$renderTable()
#' @field argumentCheckMode A number (0-4 meaning none, minimal, basic,
#'   balanced, full) indicating the argument checking level.
#' @field traceEnabled A logical value indicating whether actions are logged to
#'   a trace file.
#' @field rowCount The number of rows in the table.
#' @field columnCount The number of columns in the table.
#' @field cells A TableCells object containing all of the cells in the body of
#'   the table.
#' @field theme The name of the theme currently applied to the table.
#' @field styles A TableStyles object containing the styles used to theme the
#'   table.
#' @field allowExternalStyles Enable support for external styles, when producing
#'   content for external systems.
#' @field allTimings The time taken for various activities related to
#'   constructing the table.
#' @field significantTimings The time taken for various activities related to
#'   constructing the table, where the elapsed time > 0.1 seconds.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(argumentCheckMode="auto", traceEnabled=FALSE,
#'   traceFile=NULL)}}{Create a new table, including optionally enabling debug
#'   logging.}
#'
#'   \item{\code{addData(dataFrame=NULL, columnNamesAsColumnHeaders=TRUE,
#'   explicitColumnHeaders=NULL, rowNamesAsRowHeaders=FALSE,
#'   firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL, columnFormats=NULL,
#'   baseStyleNames=NULL)}}{Generate the table from a data frame, specifying
#'   headers and value formatting.}
#'   \item{\code{addMatrix(matrix=NULL, columnNamesAsColumnHeaders=TRUE,
#'   explicitColumnHeaders=NULL, rowNamesAsRowHeaders=FALSE,
#'   explicitRowHeaders=NULL, columnFormats=NULL,
#'   baseStyleNames=NULL)}}{Generate the table from a matrix, specifying headers
#'   and value formatting.l}
#'   \item{\code{mergeCells(rFrom, cFrom, rSpan=NULL, cSpan=NULL,
#'   rTo=NULL, cTo=NULL)}}{Merge cells in the table.  This does not delete
#'   the other cells covered by the merged cell.  When the table is output, the
#'   top-left most cell in the merged cell range is rendered over the other
#'   cells (which are effectively hidden).}
#'   \item{\code{unmergeCells(r, c, errorIfNotFound=TRUE)}}{Delete a merged cell
#'   range by specifying any of the cells covered by the merged cell.}
#'   \item{\code{applyCellMerges()}}{Updates the isMerged, isMergeRoot and
#'   mergeIndex properties of the cells.}
#'   \item{\code{formatValue(value=NULL, format=NULL)}}{Format a value for
#'   display, using either sprintf(), format() or a custom formatting function.}
#'   \item{\code{addStyle(styleName, declarations)}}{Define a new TableStyle and
#'   add it to the TableStyles collection.}
#'   \item{\code{createInlineStyle(baseStyleName, declarations)}}{Create a
#'   TableStyle object that can be used to style individual cells in the table.}
#'   \item{\code{setStyling(rFrom, cFrom, rTo=NULL, cTo=NULL,
#'   baseStyleName=NULL, style=NULL, declarations=NULL)}}{Set the style settings
#'   across a range of cells.}
#'   \item{\code{resetCells()}}{Clear the cells of the table.}
#'   \item{\code{getCells(specifyCellsAsList=FALSE, rowNumbers=NULL,
#'   columnNumbers=NULL, cellCoordinates=NULL)}}{Retrieve cells by a combination
#'   of row and/or column numbers.}
#'   \item{\code{findCells(rowNumbers=NULL, columnNumbers=NULL,
#'   minValue=NULL, maxValue=NULL, exactValues=NULL, includeNull=TRUE,
#'   includeNA=TRUE)}}{Find cells in the body of the table matching
#'   the specified criteria.}
#'   \item{\code{print(asCharacter=FALSE)}}{Either print the table to the
#'   console or retrieve it as a character value.}
#'   \item{\code{asMatrix(firstRowAsColumnNames=FALSE,
#'   firstColumnAsRowNames=FALSE, rawValue=FALSE)}}{Gets the table as
#'   a matrix, with or without headings.}
#'   \item{\code{asDataFrame(firstRowAsColumnNames=FALSE,
#'   firstColumnAsRowNames=FALSE, rawValue=FALSE)}}{Gets the table as a data
#'   frame, with or without headings.}
#'   \item{\code{getCss(styleNamePrefix)}}{Get the CSS declarations for the
#'   entire table.}
#'   \item{\code{getHtml(styleNamePrefix)}}{Get the HTML representation of the
#'   table, specifying the CSS style name prefix to use.}
#'   \item{\code{saveHtml(filePath, fullPageHTML=TRUE, styleNamePrefix)}}{Save
#'   the HTML representation of the table to a file.}
#'   \item{\code{renderTable(width, height, styleNamePrefix)}}{Render the table
#'   as a htmlwidget.}
#'   \item{\code{writeToExcelWorksheet(wb=NULL, wsName=NULL, topRowNumber=NULL,
#'   leftMostColumnNumber=NULL, mapStylesFromCSS=TRUE)}}{Output the table
#'   into the specified workbook and worksheet at the specified row-column
#'   location.}
#'   \item{\code{asList()}}{Get a list representation of the table.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the table.}
#'   \item{\code{viewJSON()}}{View the JSON representation of the table.}
#' }

BasicTable <- R6::R6Class("BasicTable",
  public = list(
    initialize = function(argumentCheckMode="auto", traceEnabled=FALSE, traceFile=NULL) {
      checkArgument(4, TRUE, "BasicTable", "initialize", argumentCheckMode, missing(argumentCheckMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "none", "minimal", "basic", "balanced", "full"))
      checkArgument(4, TRUE, "BasicTable", "initialize", traceEnabled, missing(traceEnabled), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument(4, TRUE, "BasicTable", "initialize", traceFile, missing(traceFile), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      if(argumentCheckMode=="auto") {
        if (length(strsplit(packageDescription("basictabler")$Version, "\\.")[[1]]) > 3) {
          message("Development version of basictabler detected: Using argumentCheckMode=full.\nThis may reduce performance. To override, specify the argumentCheckMode explicitly.\nargumentCheckMode values: none, minimal, basic, balanced (the normal default), full.")
          private$p_argumentCheckMode <- 4
        }
        else private$p_argumentCheckMode <- 3
      }
      else if(argumentCheckMode=="none") private$p_argumentCheckMode <- 0
      else if(argumentCheckMode=="minimal") private$p_argumentCheckMode <- 1
      else if(argumentCheckMode=="basic") private$p_argumentCheckMode <- 2
      else if(argumentCheckMode=="balanced") private$p_argumentCheckMode <- 3
      else if(argumentCheckMode=="full") private$p_argumentCheckMode <- 4
      else stop("BasicTable$initialize():  Unknown argumentCheckMode encountered.", call. = FALSE)
      private$p_traceEnabled <- traceEnabled
      if(private$p_traceEnabled&(!is.null(traceFile))) {
        private$p_traceFile <- file(traceFile, open="w")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$new", "Creating new Basic Table...")
      # Create the basic parts of the table
      private$p_styles <- getTblTheme(parentTable=self, themeName="default")
      private$p_cells <- TableCells$new(self)
      private$p_mergedCells <- TableCellRanges$new(self)
      private$p_htmlRenderer <- TableHtmlRenderer$new(parentTable=self)
      private$p_openxlsxRenderer <-TableOpenXlsxRenderer$new(parentTable=self)
      private$p_timings <- list()
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$new", "Created new Basic Table.")
      return(invisible())
    },
    addData = function(dataFrame=NULL,
                       columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                       rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL,
                       columnFormats=NULL, baseStyleNames=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addData", "Adding data to Table...")
      dfRowCount <- nrow(dataFrame)
      dfColumnCount <- ncol(dataFrame)
      # clear any cells that may be present
      private$p_cells$reset()
      # check there are some columns
      if(dfColumnCount==0) return(invisible())
      # check the formats
      if(!is.null(columnFormats)) {
        if(length(columnFormats) != dfColumnCount) {
          stop("BasicTable$addData():  Length of columnFormats must match the number of columns in the data frame!", call. = FALSE)
        }
      }
      # check the base style names
      if(!is.null(baseStyleNames)) {
        if(length(baseStyleNames) != dfColumnCount) {
          stop("BasicTable$addData():  Length of baseStyleNames must match the number of columns in the data frame!", call. = FALSE)
        }
      }
      # get the column headers
      columnHeaders <- NULL
      if(is.null(explicitColumnHeaders) && columnNamesAsColumnHeaders) {
        columnHeaders <- names(dataFrame)
      }
      else if(!is.null(explicitColumnHeaders)) {
        if(length(explicitColumnHeaders) != dfColumnCount) {
          stop("BasicTable$addData():  Length of explicitColumnHeaders must match the number of columns in the data frame!", call. = FALSE)
        }
        columnHeaders <- explicitColumnHeaders
      }
      # get the row headers
      rowHeaders <- NULL
      if(is.null(explicitRowHeaders) && rowNamesAsRowHeaders) {
        rowHeaders <- row.names(dataFrame)
      }
      else if(!is.null(explicitRowHeaders)) {
        if(length(explicitRowHeaders) != dfRowCount) {
          stop("BasicTable$addData():  Length of explicitRowHeaders must match the number of rows in the data frame!", call. = FALSE)
        }
        rowHeaders <- explicitRowHeaders
      }
      # position cursors
      cells <- private$p_cells
      rowNumber <- 0
      columnNumber <- 0
      # generate the column header row (if present)
      if(!is.null(columnHeaders)) {
        rowNumber <- rowNumber + 1
        columnNumber <- 0
        if(!is.null(rowHeaders)) {
          columnNumber <- columnNumber + 1
          cells$setBlankCell(rowNumber, columnNumber, cellType="root")
        }
        for(c in 1:length(columnHeaders)) {
          columnNumber <- columnNumber + 1
          cell <- TableCell$new(parentTable=self, cellType="columnHeader",
                                rowNumber=rowNumber, columnNumber=columnNumber,
                                rawValue=columnHeaders[c],
                                formattedValue=columnHeaders[c])
          cells$moveCell(rowNumber, columnNumber, cell)
        }
      }
      # generate the rows
      if(dfRowCount>0) {
        for(r in 1:dfRowCount) {
          rowNumber <- rowNumber + 1
          columnNumber <- 0
          if(!is.null(rowHeaders)) {
            columnNumber <- columnNumber + 1
            cell <- TableCell$new(parentTable=self, cellType="rowHeader",
                                  rowNumber=rowNumber, columnNumber=columnNumber,
                                  rawValue=rowHeaders[r],
                                  formattedValue=rowHeaders[r])
            cells$moveCell(rowNumber, columnNumber, cell)
          }
          for(c in 1:dfColumnCount) {
            columnNumber <- columnNumber + 1
            value <- dataFrame[[r, c]]
            if(is.null(columnFormats)) formattedValue <- value
            else if(is.null(columnFormats[[c]])) formattedValue <- value
            else if(is.na(columnFormats[[c]])) formattedValue <- value
            else formattedValue <- self$formatValue(value, columnFormats[[c]])
            if(firstColumnAsRowHeaders && (c==1)) cellType <- "rowHeader"
            else cellType <- "cell"
            baseStyleName <- NULL
            if(!is.null(baseStyleNames)) {
              if(!is.null(baseStyleNames[[c]])) baseStyleName <- baseStyleNames[[c]]
            }
            cell <- TableCell$new(parentTable=self, cellType=cellType,
                                  rowNumber=rowNumber, columnNumber=columnNumber,
                                  rawValue=value, formattedValue=formattedValue,
                                  baseStyleName=baseStyleName)
            cells$moveCell(rowNumber, columnNumber, cell)
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addData", "Added data to Table.")
      private$addTiming(paste0("addData()"), timeStart)
      return(invisible())
    },
    addMatrix = function(matrix=NULL,
                       columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                       rowNamesAsRowHeaders=FALSE, explicitRowHeaders=NULL, columnFormats=NULL, baseStyleNames=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", matrix, missing(matrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses="matrix")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addMatrix", "Adding matrix to Table...")
      mRowCount <- nrow(matrix)
      mColumnCount <- ncol(matrix)
      # clear any cells that may be present
      private$p_cells$reset()
      # check there are some columns
      if(mColumnCount==0) return(invisible())
      # check the formats
      if(!is.null(columnFormats)) {
        if(length(columnFormats) != mColumnCount) {
          stop("BasicTable$addMatrix():  Length of columnFormats must match the number of columns in the matrix!", call. = FALSE)
        }
      }
      # check the base style names
      if(!is.null(baseStyleNames)) {
        if(length(baseStyleNames) != mColumnCount) {
          stop("BasicTable$addData():  Length of baseStyleNames must match the number of columns in the matrix!", call. = FALSE)
        }
      }
      # get the column headers
      columnHeaders <- NULL
      if(is.null(explicitColumnHeaders) && columnNamesAsColumnHeaders) {
        columnHeaders <- colnames(matrix)
      }
      else if(!is.null(explicitColumnHeaders)) {
        if(length(explicitColumnHeaders) != mColumnCount) {
          stop("BasicTable$addMatrix():  Length of explicitColumnHeaders must match the number of columns in the matrix!", call. = FALSE)
        }
        columnHeaders <- explicitColumnHeaders
      }
      # get the row headers
      rowHeaders <- NULL
      if(is.null(explicitRowHeaders) && rowNamesAsRowHeaders) {
        rowHeaders <- rownames(matrix)
      }
      else if(!is.null(explicitRowHeaders)) {
        if(length(explicitRowHeaders) != mRowCount) {
          stop("BasicTable$addMatrix():  Length of explicitRowHeaders must match the number of rows in the matrix!", call. = FALSE)
        }
        rowHeaders <- explicitRowHeaders
      }
      # position cursors
      cells <- private$p_cells
      rowNumber <- 0
      columnNumber <- 0
      # generate the column header row (if present)
      if(!is.null(columnHeaders)) {
        rowNumber <- rowNumber + 1
        columnNumber <- 0
        if(!is.null(rowHeaders)) {
          columnNumber <- columnNumber + 1
          cells$setBlankCell(rowNumber, columnNumber, cellType="root")
        }
        for(c in 1:length(columnHeaders)) {
          columnNumber <- columnNumber + 1
          cell <- TableCell$new(parentTable=self, cellType="columnHeader",
                                rowNumber=rowNumber, columnNumber=columnNumber,
                                rawValue=columnHeaders[c],
                                formattedValue=columnHeaders[c])
          cells$moveCell(rowNumber, columnNumber, cell)
        }
      }
      # generate the rows
      if(mRowCount>0) {
        for(r in 1:mRowCount) {
          rowNumber <- rowNumber + 1
          columnNumber <- 0
          if(!is.null(rowHeaders)) {
            columnNumber <- columnNumber + 1
            cell <- TableCell$new(parentTable=self, cellType="rowHeader",
                                  rowNumber=rowNumber, columnNumber=columnNumber,
                                  rawValue=rowHeaders[r],
                                  formattedValue=rowHeaders[r])
            cells$moveCell(rowNumber, columnNumber, cell)
          }
          for(c in 1:mColumnCount) {
            columnNumber <- columnNumber + 1
            value <- matrix[[r, c]]
            if(is.null(columnFormats)) formattedValue <- value
            else if(is.null(columnFormats[[c]])) formattedValue <- value
            else if(is.na(columnFormats[[c]])) formattedValue <- value
            else formattedValue <- self$formatValue(value, columnFormats[[c]])
            baseStyleName <- NULL
            if(!is.null(baseStyleNames)) {
              if(!is.null(baseStyleNames[[c]])) baseStyleName <- baseStyleNames[[c]]
            }
            cell <- TableCell$new(parentTable=self, cellType="cell",
                                  rowNumber=rowNumber, columnNumber=columnNumber,
                                  rawValue=value, formattedValue=formattedValue,
                                  baseStyleName=baseStyleName)
            cells$moveCell(rowNumber, columnNumber, cell)
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addMatrix", "Added data to Table.")
      private$addTiming(paste0("addMatrix()"), timeStart)
      return(invisible())
    },
    mergeCells = function(rFrom=NULL, cFrom=NULL, rSpan=NULL, cSpan=NULL, rTo=NULL, cTo=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rFrom, missing(rFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cFrom, missing(cFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rSpan, missing(rSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cSpan, missing(cSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Merging cells...", list(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo))
      existingRange <- private$p_mergedCells$findIntersectingRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(!is.null(existingRange)) {
        stop(paste0("BasicTable$mergeCells(): An existing merged cell range intersects with the specified cell range."), call. = FALSE)
      }
      private$p_mergedCells$addRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Merged cells.")
      return(invisible())
    },
    unmergeCells = function(r=NULL, c=NULL, errorIfNotFound=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "unmergeCells", r, missing(r), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "unmergeCells", c, missing(c), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Unmerging cells...", list(r, c))
      rangeDeleted <- private$p_mergedCells$deleteRange(r=r, c=c)
      if(errorIfNotFound && (!rangeDeleted)) {
        stop(paste0("BasicTable$unmergeCells(): No cell range could be found that intersects the specified cell."), call. = FALSE)
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Unmerged cells.")
      return(invisible())
    },
    applyCellMerges = function() {
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$applyCellMerges", "Applying cell merges...")
      # clear existing cell merge info
      if(length(private$p_cells$rows) > 0) {
        for(r in 1:length(private$p_cells$rows)) {
          if(length(private$p_cells$rows[[r]]) > 0) {
            for(c in 1:length(private$p_cells$rows[[r]])) {
              cell <- private$p_cells$rows[[r]][[c]]
              if(!is.null(cell)) {
                cell$isMerged <- FALSE
                cell$isMergeRoot <- FALSE
                cell$mergeIndex <- NULL
              }
            }
          }
        }
      }
      # set the merged cell info onto each cell
      mergeRanges <- private$p_mergedCells$ranges
      if(length(mergeRanges) > 0) {
        for(i in 1:length(mergeRanges)) {
          mr <- mergeRanges[[i]]
          cell <- private$p_cells$rows[[mr$rFrom]][[mr$cFrom]]
          cell$isMergeRoot <- TRUE
          for(r in mr$rFrom:mr$rTo) {
            for(c in mr$cFrom:mr$cTo) {
              cell <- private$p_cells$rows[[r]][[c]]
              cell$isMerged <- TRUE
              cell$mergeIndex <- i
            }
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$applyCellMerges", "Applied cell merges.")
      return(invisible())
    },
    formatValue = function(value=NULL, format=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$formatValue", "Formatting value...")
      if(is.null(value)) return(invisible(NULL))
      if(is.null(format)) return(value)
      clsv <- class(value)
      if(("numeric" %in% clsv)||("integer" %in% clsv)||("complex" %in% clsv)) {
        clsf <- class(format)
        if("character" %in% clsf) value <- sprintf(format, value)
        else if ("list" %in% clsf) {
          args <- format
          args$x <- value
          value <- do.call(base::format, args)
        }
        else if ("function" %in% class(format)) value <- format(value)
      }
      else if(("Date" %in% clsv)||("POSIXct" %in% clsv)||("POSIXlt" %in% clsv)) {
        clsf <- class(format)
        if ("list" %in% clsf) {
          args <- format
          args$x <- value
          value <- do.call(base::format, args)
        }
        else if ("function" %in% class(format)) value <- format(value)
      }
      else if ("factor" %in% clsv) value <- as.character(value)
      else if("logical" %in% clsv) value <- as.character(value)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$formatValue", "Formated value.")
      return(invisible(value))
    },
    addStyle = function(styleName=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addStyle", "Adding style...", list(styleName=styleName))
      style <- private$p_styles$addStyle(styleName=styleName, declarations=declarations)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$addStyle", "Added style.")
      return(invisible(style))
    },
    createInlineStyle = function(baseStyleName=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "createInlineStyle", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "createInlineStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$createInlineStyle", "Creating inline style...")
      if(is.null(baseStyleName)) {
        style <- TableStyle$new(parentTable=self, styleName="", declarations=declarations)
      }
      else {
        baseStyle <- private$p_styles$getStyle(styleName=baseStyleName)
        style <- TableStyle$new(parentTable=self, styleName="", declarations=baseStyle$declarations)
        style$setPropertyValues(declarations=declarations)
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$createInlineStyle", "Created inline style.")
      return(invisible(style))
    },
    setStyling = function(rFrom=NULL, cFrom=NULL, rTo=NULL, cTo=NULL, baseStyleName=NULL, style=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", rFrom, missing(rFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", cFrom, missing(cFrom), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$setStyling", "Setting styling...")
      if(is.null(rTo)) rTo <- rFrom
      if(is.null(cTo)) cTo <- cFrom
      if(rTo<rFrom) { stop("BasicTable$setStyling():  rTo must be greater than or equal to rFrom.", call. = FALSE) }
      if(cTo<cFrom) { stop("BasicTable$setStyling():  cTo must be greater than or equal to cFrom.", call. = FALSE) }
      if(missing(baseStyleName)&&missing(style)&&missing(declarations)) { stop("BasicTable$setStyling():  Please specify at least one of baseStyleName, style or declarations.", call. = FALSE) }
      if((!is.null(style))&&(!is.null(declarations))) { stop("BasicTable$setStyling():  Please specify either style or declarations, not both.", call. = FALSE) }
      for(r in rFrom:rTo) {
        for(c in cFrom:cTo) {
          cell <- self$cells$getCell(r, c)
          if(!is.null(cell)) {
            if(!missing(baseStyleName)) { cell$baseStyleName <- baseStyleName }
            if(!missing(style)) { cell$style <- style$getCopy() }
            if(!missing(declarations)) { cell$style <- TableStyle$new(parentTable=self, declarations=declarations) }
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$setStyling", "Set styling.")
    },
    resetCells = function() {
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$resetCells", "Resetting cells...")
      if(private$p_evaluated==TRUE){
        timeStart <- proc.time()
        private$p_cells$reset()
        private$addTiming("resetCells", timeStart)
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$resetCells", "Reset cells.")
      return(invisible())
    },
    getCells = function(specifyCellsAsList=FALSE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCells", "Getting cells...")
      if(is.null(private$p_cells)) stop("BasicTable$getCells():  No cells exist to retrieve.", call. = FALSE)
      # need to miss the specifyCellsAsList argument out if it is missing here, so the warning message is generated
      cells <- private$p_cells$getCells(specifyCellsAsList=specifyCellsAsList, rowNumbers=rowNumbers, columnNumber=columnNumbers, cellCoordinates=cellCoordinates)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCells", "Got cells.")
      return(invisible(cells))
    },
    findCells = function(rowNumbers=NULL, columnNumbers=NULL,
                         minValue=NULL, maxValue=NULL, exactValues=NULL, includeNull=TRUE, includeNA=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$findCells", "Finding cells...")
      if(is.null(private$p_cells)) stop("BasicTable$findCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$findCells(rowNumbers=rowNumbers, columnNumbers=columnNumbers,
                                         minValue=minValue, maxValue=maxValue, exactValues=exactValues, includeNull=includeNull, includeNA=includeNA)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$findCells", "Found cells.")
      return(invisible(cells))
    },
    print = function(asCharacter=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "print", asCharacter, missing(asCharacter), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$print", "Printing...")
      lineIndex <- 0
      if(asCharacter==TRUE) returnLines <- vector("list", self$rowCount)
      else returnLines <- NULL
      # constant
      columnPadding <- 2 # characters
      # column widths
      columnWidths <- vector("integer", self$columnCount)
      # get the cells
      cells <- private$p_cells
      if(cells$rowCount == 0) return(invisible())
      if(cells$columnCount == 0) return(invisible())
      # get the column widths
      if(length(cells$rows) > 0) {
        for(r in 1:length(cells$rows)) {
          if(length(cells$rows[[r]]) > 0) {
            for(c in 1:length(cells$rows[[r]])) {
              cell <- cells$getCell(r, c)
              if(is.null(cell)) next
              if(is.null(cell$formattedValue)) next
              if(length(cell$formattedValue)==0) next
              if(is.na(cell$formattedValue)) next
              columnWidths[c] <- max(columnWidths[c], nchar(cell$formattedValue))
            }
          }
        }
      }
      for(c in 1:length(columnWidths)) {
        columnWidths[c] <- columnWidths[c] + 2
      }
      # quick functions
      repStr <- function(string, times) {
        return(paste(rep(string, times), collapse = ""))
      }
      padStr <- function(str, len, side){
        if(is.null(str)) return(repStr(" ", len))
        thisLength <- nchar(str)
        padLength <- len - thisLength
        if(side=="left") return(paste0(repStr(" ", padLength), str))
        else return(paste0(str, repStr(" ", padLength)))
      }
      # output the cells
      if(length(cells$rows) > 0) {
        for(r in 1:length(cells$rows)) {
          currentLine <- NULL
          if(length(cells$rows[[r]]) > 0) {
            for(c in 1:length(cells$rows[[r]])) {
              cell <- cells$getCell(r, c)
              if(is.null(cell)) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else if(is.null(cell$formattedValue)) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else if(length(cell$formattedValue)==0) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else if(is.na(cell$formattedValue)) currentLine <- paste0(currentLine, repStr(" ", columnWidths[c]))
              else currentLine <- paste0(currentLine, repStr(" ", columnWidths[c] - 2 - nchar(cell$formattedValue)), cell$formattedValue, "  ")
            }
          }
          # print this line
          if(asCharacter==TRUE) {
            lineIndex <- lineIndex + 1
            returnLines[[lineIndex]] <- currentLine
          }
          else cat(paste0(currentLine, "\n"))
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$print", "Printed.")
      return(invisible(paste0(returnLines, sep="", collapse="\n")))
    },
    asMatrix = function(firstRowAsColumnNames=FALSE, firstColumnAsRowNames=FALSE, rawValue=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asMatrix", firstRowAsColumnNames, missing(firstRowAsColumnNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asMatrix", firstColumnAsRowNames, missing(firstColumnAsRowNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asMatrix", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asMatrix", "Getting table as a matrix...",
                    list(includeHeaders=includeHeaders, rawValue=rawValue))
      if(is.null(private$p_cells)) stop("BasicTable$asMatrix():  No cells exist to retrieve.", call. = FALSE)
      # size the matrix
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      if((rowCount==0)||(columnCount==0)) return(matrix())
      # offsets
      if(firstColumnAsRowNames) columnOffset <- 1
      else columnOffset <- 0
      if(firstRowAsColumnNames) rowOffset <- 1
      else rowOffset <- 0
      # column names
      columnNames <- NULL
      if(firstRowAsColumnNames) {
        columnNames <- vector("character", columnCount - columnOffset)
        if(length(private$p_cells$rows[[1]])>0) {
          for(c in (1 + columnOffset):columnCount) {
            cell <- private$p_cells$getCell(1, c)
            if(is.null(cell)) next
            columnNames[c - columnOffset] <- cell$formattedValue
          }
        }
      }
      # row names
      rowNames <- NULL
      if(firstColumnAsRowNames) {
        rowNames <- vector("character", rowCount - rowOffset)
        for(r in (1 + rowOffset):rowCount) {
          if(length(private$p_cells$rows[[r]])==0) next
          cell <- private$p_cells$getCell(r, 1)
          if(is.null(cell)) next
          rowNames[r - rowOffset] <- cell$formattedValue
        }
      }
      # populate the matrix
      m <- matrix(data=NA, nrow=rowCount - rowOffset, ncol=columnCount - columnOffset)
      # set the cell values
      for(r in (1 + rowOffset):rowCount) {
        if(length(private$p_cells$rows[[r]])==0) next
        for(c in (1 + columnOffset):columnCount) {
          cell <- private$p_cells$getCell(r, c)
          if(is.null(cell)) next
          if(rawValue==TRUE) v <- cell$rawValue
          else v <- cell$formattedValue
          if(is.null(v)) v <- NA
          else if(is.factor(v)) v <- as.character(v)
          m[r - rowOffset, c - columnOffset] <- v
        }
      }
      # set the names
      if(!is.null(rowNames)) rownames(m) <- rowNames
      if(!is.null(columnNames)) colnames(m) <- columnNames
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asMatrix", "Got table as a matrix.")
      return(m)
    },
    asDataFrame = function(firstRowAsColumnNames=FALSE, firstColumnAsRowNames=FALSE, rawValue=FALSE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", firstRowAsColumnNames, missing(firstRowAsColumnNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", firstColumnAsRowNames, missing(firstColumnAsRowNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asDataFrame", "Getting table as a data frame...",
                    list(includeHeaders=includeHeaders, rawValue=rawValue))
      if(is.null(private$p_cells)) stop("BasicTable$asDataFrame():  No cells exist to retrieve.", call. = FALSE)
      # size the data frame
      rowCount <- private$p_cells$rowCount
      columnCount <- private$p_cells$columnCount
      if((rowCount==0)||(columnCount==0)) return(matrix())
      # offsets
      if(firstColumnAsRowNames) columnOffset <- 1
      else columnOffset <- 0
      if(firstRowAsColumnNames) rowOffset <- 1
      else rowOffset <- 0
      # column names
      columnNames <- NULL
      if(firstRowAsColumnNames) {
        columnNames <- vector("character", columnCount - columnOffset)
        if(length(private$p_cells$rows[[1]])>0) {
          for(c in (1 + columnOffset):columnCount) {
            cell <- private$p_cells$getCell(1, c)
            if(is.null(cell)) next
            columnNames[c - columnOffset] <- cell$formattedValue
          }
        }
      }
      # row names
      rowNames <- NULL
      if(firstColumnAsRowNames) {
        rowNames <- vector("character", rowCount - rowOffset)
        for(r in (1 + rowOffset):rowCount) {
          if(length(private$p_cells$rows[[r]])==0) next
          cell <- private$p_cells$getCell(r, 1)
          if(is.null(cell)) next
          rowNames[r - rowOffset] <- cell$formattedValue
        }
      }
      # get the cell values
      dfColumns <- list()
      for(c in (1 + columnOffset):columnCount) {
        columnValues <- NA
        for(r in (1 + rowOffset):rowCount) {
          if(length(private$p_cells$rows[[r]])==0) next
          cell <- private$p_cells$getCell(r, c)
          if(is.null(cell)) next
          if(rawValue==TRUE) v <- cell$rawValue
          else v <- cell$formattedValue
          if(is.null(v)) v <- NA
          else if(is.factor(v)) v <- as.character(v)
          columnValues[r - rowOffset] <- v
        }
        dfColumns[[c - columnOffset]] <- columnValues
      }
      # set the names
      if(is.null(columnNames)) {
        columnNames <- paste0("C", sprintf("%03d", 1:(columnCount - columnOffset)))
      }
      df <- as.data.frame(dfColumns, col.names=columnNames)
      if(!is.null(rowNames)) rownames(df) <- rowNames
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asDataFrame", "Got table as a data frame.")
      return(df)
    },
    getCss = function(styleNamePrefix=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCss", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCss", "Getting Styles...")
      if(is.null(private$p_styles)) return("")
      if(length(private$p_styles$styles)==0) return("")
      styles <- ""
      for(s in 1:length(private$p_styles$styles)) {
        style <- private$p_styles$styles[[s]]
        if(is.null(style)) next
        styles <- paste0(styles, style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix), "\r\n")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCss", "Got Styles.")
      private$addTiming("getCss", timeStart)
      return(invisible(styles))
    },
    getHtml = function(styleNamePrefix=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getHtml", "Getting HTML...")
      if(is.null(private$p_cells)) stop("BasicTable$getHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getHtml", "Got HTML.")
      private$addTiming("getHtml", timeStart)
      return(invisible(htmlTable))
    },
    saveHtml = function(filePath=NULL, fullPageHTML=TRUE, styleNamePrefix=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "saveHtml", filePath, missing(filePath), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "saveHtml", fullPageHTML, missing(fullPageHTML), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "saveHtml", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$saveHtml", "Saving HTML...", list(filePath=filePath, fullPageHTML=fullPageHTML))
      if(is.null(private$p_cells)) stop("BasicTable$saveHtml():  No cells exist to render.", call. = FALSE)
      htmlTable <- private$p_htmlRenderer$getTableHtml(styleNamePrefix=styleNamePrefix)
      if (fullPageHTML==FALSE) {
        fileConn <- file(filePath)
        writeLines(as.character(htmlTable), fileConn)
        close(fileConn)
        if(private$p_traceEnabled==TRUE) self$trace("BasicTable$saveHtml", "Saved HTML.")
        return(invisible())
      }
      # basic css
      cssStr1 <- "<style>h1 { font: 2.5em arial; font-weight: bold; } p { font: 0.9em arial; }</style>"
      cssStr2 <- paste0("<style>", self$getCss(styleNamePrefix=styleNamePrefix), "</style>")
      #pgHtml <- htmltools::tags$html(htmltools::tags$head(htmltools::tags$title('R Table')), htmltools::HTML(cssStr),
      pgHtml <- htmltools::tags$html(htmltools::HTML("<head>"), htmltools::tags$title('R Table'), htmltools::HTML(cssStr1), htmltools::HTML(cssStr2), htmltools::HTML("</head>"),
                 htmltools::tags$body(
                   htmltools::h1("R Table"),
                   htmlTable,
                   htmltools::tags$br(),
                   htmltools::tags$p(paste0("Generated at ", format(Sys.time(), "%X on %a %d %b %Y")))
                 ))
      fileConn <- file(filePath)
      writeLines(as.character(pgHtml), fileConn)
      close(fileConn)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$saveHtml", "Saved HTML.")
      return(invisible())
    },
    renderTable = function(width=NULL, height=NULL, styleNamePrefix=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "renderTable", width, missing(width), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "renderTable", height, missing(height), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "renderTable", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$renderTable", "Rendering htmlwidget...", list(width=width, height=height, styleNamePrefix=styleNamePrefix))
      settings <- list() # may need this in the future
      widgetData <- list(
        tableCss = self$getCss(styleNamePrefix=styleNamePrefix),
        tableHtml = as.character(self$getHtml(styleNamePrefix=styleNamePrefix)),
        settings = settings
      )
      # viewer.fill=TRUE and browser.fill=TRUE sound like they would be good things, but they seem to prevent
      # any scroll bars being shown when the HTML tables are larger than the RStudio Viewer or the web browser window size
      sp = htmlwidgets::sizingPolicy(
        viewer.padding=10, viewer.fill=FALSE, viewer.suppress=FALSE,
        browser.padding=10, browser.fill=FALSE,
        knitr.defaultWidth="auto", knitr.defaultHeight="auto", knitr.figure = FALSE
      )
      w <- htmlwidgets::createWidget("basictabler", widgetData, width=width, height=height, sizingPolicy=sp)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$renderTable", "Rendered htmlwidget.")
      return(w)
    },
    writeToExcelWorksheet = function(wb=NULL, wsName=NULL, topRowNumber=NULL, leftMostColumnNumber=NULL, outputValuesAs="rawValue", applyStyles=TRUE, mapStylesFromCSS=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", wb, missing(wb), allowMissing=TRUE, allowNull=TRUE, allowedClasses="Workbook")
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", wsName, missing(wsName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", topRowNumber, missing(topRowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", leftMostColumnNumber, missing(leftMostColumnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", outputValuesAs, missing(outputValuesAs), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("rawValue", "formattedValueAsText", "formattedValueAsNumber"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "writeToExcelWorksheet", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("BasicTable$writeToExcelWorksheet():  The openxlsx package is needed to write the table to an Excel file.  Please install it.", call. = FALSE)
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$writeToExcelWorksheet", "Writing to worksheet...")
      private$p_openxlsxRenderer$writeToWorksheet(wb=wb, wsName=wsName, topRowNumber=topRowNumber,
                                                  leftMostColumnNumber=leftMostColumnNumber,
                                                  outputValuesAs=outputValuesAs,
                                                  applyStyles=applyStyles, mapStylesFromCSS=mapStylesFromCSS)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$writeToExcelWorksheet", "Written to worksheet.")
    },
    trace = function(methodName, desc, detailList=NULL) {
      if(!private$p_traceEnabled) return()
      stackdepth <- length(sys.calls())
      repStr <- function(string, times) {
        return(paste(rep(string, times), collapse = ""))
      }
      indent <- repStr(" ", (stackdepth - 1) *2)
      msg <- paste0(indent, methodName, ":  ", desc)
      if(length(detailList)>0) {
        nms <- names(detailList)
        msg <- paste0(msg, ": ")
        for(i in 1:length(detailList)) {
          sep <- ""
          if(i > 1) { sep <- ", " }
          dtl <- NULL
          if("function" %in% class(detailList[[i]])) {
            dtl <- deparse(detailList[[i]])
          }
          else dtl <- detailList[[i]]
          msg <- paste0(msg, sep, nms[i], "=", dtl)
        }
      }
      if(is.null(private$p_traceFile)) { message(msg) }
      else { cat(msg, file=private$p_traceFile, sep="\r\n", append=TRUE)}
    },
    asList = function() {
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asList", "Getting list...")
      lst <- list(
        type = "BasicTable"
      )
      if(!is.null(private$p_cells)) lst$cells <- private$p_cells$asList()
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asList", "Got list.")
      return(lst)
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    viewJSON = function() {
      if (!requireNamespace("listviewer", quietly = TRUE)) {
        stop("BasicTable$asJSON():  The listviewer package is needed to view the internal structure of the BasicTable as JSON.  Please install it.", call. = FALSE)
      }
      listviewer::jsonedit(self$asList(), mode="code")
    },
    finalize = function() {
      if(!is.null(private$p_traceFile)) close(private$p_traceFile)
    }
  ),
  active = list(
    argumentCheckMode = function(value) { return(private$p_argumentCheckMode) },
    traceEnabled = function(value){
      if(missing(value)) return(invisible(private$p_traceEnabled))
      else {
        if(is.logical(value)) private$p_traceEnabled <- value
        else stop("BasicTable$traceEnabled: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },
    cells = function(value) { return(invisible(private$p_cells)) },
    mergedCells = function(value) { return(invisible(private$p_mergedCells)) },
    rowCount = function(value) { return(invisible(private$p_cells$rowCount)) },
    columnCount = function(value) { return(invisible(private$p_cells$columnCount)) },
    asCharacter = function() { return(self$print(asCharacter=TRUE)) },
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "TableStyles"))
        }
        if("character" %in% class(value)) private$p_styles <- getTblTheme(parentTable=self, themeName=value)
        else if("TableStyles" %in% class(value)) private$p_styles <- value
        return(invisible())
      }
    },
    styles = function(value) {
      if(missing(value)) return(invisible(private$p_styles))
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "styles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="TableStyles")
        }
        private$p_styles <- value
        return(invisible())
      }
    },
    allowExternalStyles = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(private$p_styles$allowExternalStyles)
      }
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        }
        private$p_styles$allowExternalStyles <- value
        return(invisible())
      }
    },
    allTimings = function(value) {
      descriptions <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$desc), NA, x$desc)) })
      user <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["user.self"]), NA, x$time["user.self"])) })
      system <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["sys.self"]), NA, x$time["sys.self"])) })
      elapsed <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["elapsed"]), NA, x$time["elapsed"])) })
      return(data.frame(action=descriptions, user=user, system=system, elapsed=elapsed))
    },
    significantTimings = function(value) {
      df <- self$allTimings
      df <- df[df$elapsed>0.1, ]
      return(df)
    }
  ),
  private = list(
    p_argumentCheckMode = 4,
    p_traceEnabled = FALSE,
    p_processingLibrary = NULL,
    p_data = NULL,
    p_styles = NULL,
    p_cells = NULL,
    p_mergedCells = NULL,
    p_htmlRenderer = NULL,
    p_openxlsxRenderer = NULL,
    p_traceFile = NULL,
    p_timings = NULL,
    # simple mechanism to track the activities/time taken to construct the table
    # should only be tracking top-level actions (i.e. methods on the table) not functions which return a value (as these could be called multiple times)
    addTiming = function(descr, timeStart) {
      timeEnd <- proc.time()
      private$p_timings[[length(private$p_timings)+1]] <- list(descr=descr, time=timeEnd-timeStart)
    }
  )
)
