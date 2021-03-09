#' R6 class that defines a basic table.
#'
#' @description
#' The `BasicTable` class represents a table with styling and formatting that can
#' be rendered to multiple output formats.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import htmlwidgets
#' @import htmltools
#' @export
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

BasicTable <- R6::R6Class("BasicTable",
  public = list(

    #' @description
    #' Create a new `BasicTable` object.
    #' @param argumentCheckMode The level of argument checking to perform.
    #' Must be one of "auto", "none", "minimal", "basic", "balanced" (default)
    #' or "full".
    #' @param theme A theme to use to style the table. Either:\cr
    #' (1) The name of a built in theme, or\cr
    #' (2) A list of simple style settings, or\cr
    #' (3) A `TableStyles` object containing a full set of styles.\cr
    #' See the "Styling" vignette for many examples.
    #' @param replaceExistingStyles Default `FALSE` to retain existing styles in
    #' the styles collection and add specified styles as new custom styles.
    #' Specify `TRUE` to update the definitions of existing styles.
    #' @param tableStyle Styling to apply to the table.  Either:\cr
    #' (1) The name of a built in style, or\cr
    #' (2) A list of CSS style declarations, e.g.\cr
    #' `list("font-weight"="bold", "color"="#0000FF")`, or\cr
    #' (3) A `TableStyle` object.
    #' @param headingStyle Styling to apply to the headings.
    #' See the `tableStyle` argument for details.
    #' @param cellStyle Styling to apply to the normal cells.
    #' See the `tableStyle` argument for details.
    #' @param totalStyle Styling to apply to the total cells.
    #' See the `tableStyle` argument for details.
    #' @param compatibility A list containing compatibility options to force
    #' legacy behaviours.  See the NEWS file for details.
    #' @param traceEnabled Default `FALSE`.  Specify `TRUE` to generate a trace
    #' for debugging purposes.
    #' @param traceFile If tracing is enabled, the location to generate the trace file.
    #' @return No return value.
    initialize = function(argumentCheckMode="auto", theme=NULL, replaceExistingStyles=FALSE,
                          tableStyle=NULL, headingStyle=NULL, cellStyle=NULL, totalStyle=NULL,
                          compatibility=NULL, traceEnabled=FALSE, traceFile=NULL) {
      checkArgument(4, TRUE, "BasicTable", "initialize", argumentCheckMode, missing(argumentCheckMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("auto", "none", "minimal", "basic", "balanced", "full"))
      checkArgument(4, TRUE, "BasicTable", "initialize", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyles"), allowedListElementClasses="character")
      checkArgument(4, TRUE, "BasicTable", "initialize", replaceExistingStyles, missing(replaceExistingStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      checkArgument(4, TRUE, "BasicTable", "initialize", tableStyle, missing(tableStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
      checkArgument(4, TRUE, "BasicTable", "initialize", headingStyle, missing(headingStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
      checkArgument(4, TRUE, "BasicTable", "initialize", cellStyle, missing(cellStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
      checkArgument(4, TRUE, "BasicTable", "initialize", totalStyle, missing(totalStyle), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "TableStyle"))
      checkArgument(4, TRUE, "BasicTable", "initialize", compatibility, missing(compatibility), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric", "logical"))
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
      private$p_compatibility <- compatibility
      private$p_traceEnabled <- traceEnabled
      if(private$p_traceEnabled&(!is.null(traceFile))) {
        private$p_traceFile <- file(traceFile, open="w")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$new", "Creating new Basic Table...")
      # Create the basic parts of the table
      private$p_lastInstanceId <- 0
      private$p_cells <- TableCells$new(self)
      private$p_mergedCells <- TableCellRanges$new(self)
      private$p_htmlRenderer <- TableHtmlRenderer$new(parentTable=self)
      private$p_openxlsxRenderer <-TableOpenXlsxRenderer$new(parentTable=self)
      private$p_timings <- list()
      # apply theming and styles
      if(is.null(theme)) {
        private$p_styles <- getTblTheme(parentTable=self, themeName="default")
      }
      else {
        if("TableStyles" %in% class(theme)) { private$p_styles <- theme }
        else if("list" %in% class(theme)) {
          private$p_styles <- getSimpleColoredTblTheme(parentTable=self, themeName="coloredTheme", colors=theme, fontName=theme$fontName)
        }
        else if("character" %in% class(theme)) {
          if(tolower(trimws(theme))=="none") { theme <- "blank" }
          private$p_styles <- getTblTheme(parentTable=self, themeName=theme)
        }
      }
      if(!is.null(tableStyle)) {
        if("TableStyle" %in% class(tableStyle)) { tableStyle <- tableStyle$declarations }
        if("list" %in% class(tableStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$tableStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$tableStyle)$setPropertyValues(declarations=tableStyle)
            tableStyle <- private$p_styles$tableStyle
          }
          else {
            private$p_styles$addStyle(styleName="customTableStyle", declarations=tableStyle)
            tableStyle <- "customTableStyle"
          }
        }
        if("character" %in% class(tableStyle)) { private$p_styles$tableStyle <- tableStyle }
      }
      if(!is.null(headingStyle)) {
        if("TableStyle" %in% class(headingStyle)) { headingStyle <- headingStyle$declarations }
        # root style
        rootStyle <- headingStyle
        if("list" %in% class(rootStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$rootStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$rootStyle)$setPropertyValues(declarations=rootStyle)
            rootStyle <- private$p_styles$rootStyle
          }
          else {
            private$p_styles$addStyle(styleName="customRootStyle", declarations=rootStyle)
            rootStyle <- "customRootStyle"
          }
        }
        if("character" %in% class(rootStyle)) { private$p_styles$rootStyle <- rootStyle }
        # row heading style
        rowHeaderStyle <- headingStyle
        if("list" %in% class(rowHeaderStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$rowHeaderStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$rowHeaderStyle)$setPropertyValues(declarations=rowHeaderStyle)
            rowHeaderStyle <- private$p_styles$rowHeaderStyle
          }
          else {
            private$p_styles$addStyle(styleName="customRowHeadingStyle", declarations=rowHeaderStyle)
            rowHeaderStyle <- "customRowHeadingStyle"
          }
        }
        if("character" %in% class(rowHeaderStyle)) { private$p_styles$rowHeaderStyle <- rowHeaderStyle }
        # column heading style
        colHeaderStyle <- headingStyle
        if("list" %in% class(colHeaderStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$colHeaderStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$colHeaderStyle)$setPropertyValues(declarations=colHeaderStyle)
            colHeaderStyle <- private$p_styles$colHeaderStyle
          }
          else {
            private$p_styles$addStyle(styleName="customColHeadingStyle", declarations=colHeaderStyle)
            colHeaderStyle <- "customColHeadingStyle"
          }
        }
        if("character" %in% class(colHeaderStyle)) { private$p_styles$colHeaderStyle <- colHeaderStyle }
      }
      if(!is.null(cellStyle)) {
        if("TableStyle" %in% class(cellStyle)) { cellStyle <- cellStyle$declarations }
        if("list" %in% class(cellStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$cellStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$cellStyle)$setPropertyValues(declarations=cellStyle)
            cellStyle <- private$p_styles$cellStyle
          }
          else {
            private$p_styles$addStyle(styleName="customCellStyle", declarations=cellStyle)
            cellStyle <- "customCellStyle"
          }
        }
        if("character" %in% class(cellStyle)) { private$p_styles$cellStyle <- cellStyle }
      }
      if(!is.null(totalStyle)) {
        if("TableStyle" %in% class(totalStyle)) { totalStyle <- totalStyle$declarations }
        if("list" %in% class(totalStyle)) {
          if(private$p_styles$isExistingStyle(private$p_styles$totalStyle)&&(!replaceExistingStyles)) {
            private$p_styles$getStyle(private$p_styles$totalStyle)$setPropertyValues(declarations=totalStyle)
            totalStyle <- private$p_styles$totalStyle
          }
          else {
            private$p_styles$addStyle(styleName="customTotalStyle", declarations=totalStyle)
            totalStyle <- "customTotalStyle"
          }
        }
        if("character" %in% class(totalStyle)) { private$p_styles$totalStyle <- totalStyle }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$new", "Created new Basic Table.")
      return(invisible())
    },

    #' @description
    #' Get the next unique object instance identifier.
    #' @details
    #' R6 classes cannot be easily compared to check if two variables are both
    #' referring to the same object instance.  Instance ids are a mechanism to
    #' work around this problem.  Each cell is assigned an
    #' instance id during object creation, which enables reliable reference
    #' comparisons.
    #' @return An integer instance id.
    getNextInstanceId = function() { # used for reliable object instance comparisons (since R6 cannot easily compare object instances)
      private$p_lastInstanceId <- private$p_lastInstanceId + 1
      return(invisible(private$p_lastInstanceId))
    },

    #' @description
    #' Populate the table from a data frame, specifying headers and value
    #'   formatting.
    #' @param dataFrame The data frame to generate the table from.
    #' @param columnNamesAsColumnHeaders `TRUE` to use the data frame column names
    #'   as column headings in the table.  Default value `TRUE.`
    #' @param explicitColumnHeaders A character vector of column names to use as
    #'   column headings in the table.
    #' @param rowNamesAsRowHeaders `TRUE` to use the data frame row names as row
    #'   headings in the table.  Default value `FALSE.`
    #' @param firstColumnAsRowHeaders `TRUE` to use the first column in the data
    #'   frame as row headings in the table.  Default value `FALSE.`
    #' @param explicitRowHeaders A character vector of row names to use as row
    #'   headings in the table.
    #' @param columnFormats A list that is the same length as the number of
    #'   columns in the data frame, where each list element specifies how to
    #'   format the values.  Each list element can be either a
    #'   character format string to be used with `sprintf()`, a list of
    #'   arguments to be used with `base::format()` or a custom R function which
    #'   will be invoked once per value to be formatted.
    #' @param baseStyleNames A character vector of style names (from the table
    #'   theme) used to style the column values.
    #' @param fmtFuncArgs A list that is the same length as the number of
    #'   columns in the data frame, where each list element specifies a list of
    #'   arguments to pass to custom R format functions.
    #' @return No return value.
    addData = function(dataFrame=NULL,
                       columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                       rowNamesAsRowHeaders=FALSE, firstColumnAsRowHeaders=FALSE, explicitRowHeaders=NULL,
                       columnFormats=NULL, baseStyleNames=NULL, fmtFuncArgs=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", dataFrame, missing(dataFrame), allowMissing=FALSE, allowNull=FALSE, allowedClasses="data.frame")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", firstColumnAsRowHeaders, missing(firstColumnAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addData", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
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
            else formattedValue <- self$formatValue(value, columnFormats[[c]], fmtFuncArgs[[c]])
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

    #' @description
    #' Populate the table from a matrix, specifying headers and value
    #'   formatting.
    #' @param matrix The matrix to generate the table from.
    #' @param columnNamesAsColumnHeaders `TRUE` to use the matrix column names
    #'   as column headings in the table.  Default value `TRUE.`
    #' @param explicitColumnHeaders A character vector of column names to use as
    #'   column headings in the table.
    #' @param rowNamesAsRowHeaders `TRUE` to use the matrix row names as row
    #'   headings in the table.  Default value `FALSE.`
    #' @param firstColumnAsRowHeaders `TRUE` to use the first column in the matrix
    #'   as row headings in the table.  Default value `FALSE.`
    #' @param explicitRowHeaders A character vector of row names to use as row
    #'   headings in the table.
    #' @param columnFormats A list that is the same length as the number of
    #'   columns in the matrix, where each list element specifies how to
    #'   format the values.  Each list element can be either a
    #'   character format string to be used with `sprintf()`, a list of
    #'   arguments to be used with `base::format()` or a custom R function which
    #'   will be invoked once per value to be formatted.
    #' @param baseStyleNames A character vector of style names (from the table
    #'   theme) used to style the column values.
    #' @param fmtFuncArgs A list that is the same length as the number of
    #'   columns in the data frame, where each list element specifies a list of
    #'   arguments to pass to custom R format functions.
    #' @return No return value.
    addMatrix = function(matrix=NULL,
                       columnNamesAsColumnHeaders=TRUE, explicitColumnHeaders=NULL,
                       rowNamesAsRowHeaders=FALSE, explicitRowHeaders=NULL,
                       columnFormats=NULL, baseStyleNames=NULL, fmtFuncArgs=NULL) {
      timeStart <- proc.time()
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", matrix, missing(matrix), allowMissing=FALSE, allowNull=FALSE, allowedClasses="matrix")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", columnNamesAsColumnHeaders, missing(columnNamesAsColumnHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", explicitColumnHeaders, missing(explicitColumnHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", rowNamesAsRowHeaders, missing(rowNamesAsRowHeaders), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", explicitRowHeaders, missing(explicitRowHeaders), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", columnFormats, missing(columnFormats), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", baseStyleNames, missing(baseStyleNames), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "addMatrix", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
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
            else formattedValue <- self$formatValue(value, columnFormats[[c]], fmtFuncArgs[[c]])
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

    #' @description
    #' Merge table cells by specifying either:\cr
    #' The top left cell (rFrom, cFrom) and the merged cell size (rSpan, cSpan) or,
    #' The top left cell (rFrom, cFrom) and bottom-right cell (rTo, cTo), or
    #' The ranges of rows/columns as vectors using rowNumbers and columnNumbers.
    #' @param rFrom The row-number of the top-left cell being merged.
    #' @param cFrom The column number of the top-left cell being merged.
    #' @param rSpan The number of rows that the merged cell spans.
    #' @param cSpan The number of columns that the merged cell spans.
    #' @param rTo The row-number of the bottom-right cell being merged.
    #' @param cTo The column-number of the bottom-right cell being merged.
    #' @param rowNumbers A vector specifying the row numbers of the cells to be merged.
    #' @param columnNumbers A vector specifying the columns numbers of the cells to be merged.
    #' @return No return value.
    mergeCells = function(rFrom=NULL, cFrom=NULL, rSpan=NULL, cSpan=NULL, rTo=NULL, cTo=NULL, rowNumbers=NULL, columnNumbers=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rFrom, missing(rFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cFrom, missing(cFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rSpan, missing(rSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cSpan, missing(cSpan), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, FALSE, "BasicTable", "mergeCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Merging cells...", list(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo, rowNumbers=rowNumbers, columnNumbers=columnNumbers))
      # determine the top-left cell
      if(is.null(rFrom)) rFrom <- min(rowNumbers)
      if(is.null(rFrom)) stop("BasicTable$mergeCells(): The row number of the top-left cell of the cells to be merged must be specified (using rFrom or rowNumbers).", call. = FALSE)
      if(is.null(cFrom)) cFrom <- min(columnNumbers)
      if(is.null(cFrom)) stop("BasicTable$mergeCells(): The column number of the top-left cell of the cells to be merged must be specified (using cFrom or columnNumbers).", call. = FALSE)
      # determine the bottom-right cell
      if(is.null(rTo)) {
        if(!is.null(rSpan)) rTo <- rFrom + rSpan - 1
        else rTo <- max(rowNumbers)
      }
      if(is.null(rTo)) stop("BasicTable$mergeCells(): The row number of the bottom-right cell of the cells to be merged must be specified (using rTo or rowNumbers, or indirectly via rSpan).", call. = FALSE)
      if(is.null(cTo)) {
        if(!is.null(cSpan)) cTo <- cFrom + cSpan - 1
        else cTo <- max(columnNumbers)
      }
      if(is.null(cTo)) stop("BasicTable$mergeCells(): The column number of the bottom-right cell of the cells to be merged must be specified (using cTo or columnNumbers, or indirectly via cSpan).", call. = FALSE)
      # determine the span
      if(is.null(rSpan)) rSpan <- rTo - rFrom + 1
      if(is.null(cSpan)) cSpan <- cTo - cFrom + 1
      # check we actually have some cells to merge
      isRealMerge <- FALSE
      isRealMerge <- isRealMerge || ((!is.null(rSpan)) && (rSpan > 1))
      isRealMerge <- isRealMerge || ((!is.null(cSpan)) && (cSpan > 1))
      if(!isRealMerge) { return(invisible()) }
      # check no intersecting merged cell
      existingRange <- private$p_mergedCells$findIntersectingRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(!is.null(existingRange)) {
        stop(paste0("BasicTable$mergeCells(): An existing merged cell range intersects with the specified cell range."), call. = FALSE)
      }
      private$p_mergedCells$addRange(rFrom=rFrom, cFrom=cFrom, rSpan=rSpan, cSpan=cSpan, rTo=rTo, cTo=cTo)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mergeCells", "Merged cells.")
      return(invisible())
    },

    #' @description
    #' Unmerge a set of merged cells by specifying any cell within the set of
    #' merged cells.
    #' @param r The row number of any cell within the merged cell.
    #' @param c The column number of any cell within the merged cell.
    #' @param errorIfNotFound `TRUE` to ignore any attempt to unmerge a cell that
    #'   is not merged.  Default value `TRUE.`
    #' @return A new `TableCell` object.
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

    #' @description
    #' Internal method that sets the `isMerged` and `mergeIndex` properties on
    #' each cell based on the cell merges that have been specified.
    #' @return No return value.
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

    #' @description
    #' Format a value using a variety of different methods.
    #' @param value The value to format.
    #' @param format Either a character format string to be used with `sprintf()`,
    #' a list of arguments to be used with `base::format()` or a custom R function
    #' which will be invoked once per value to be formatted.
    #' @param fmtFuncArgs If `format` is a custom R function, then `fmtFuncArgs`
    #' specifies any additional arguments (in the form of a list) that will be
    #' passed to the custom function.
    #' @return The formatted value if `format` is specified, otherwise the `value`
    #' converted to a character value.
    formatValue = function(value=NULL, format=NULL, fmtFuncArgs=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "formatValue", value, missing(value), allowMissing=FALSE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "complex", "character", "factor", "Date", "POSIXct", "POSIXlt"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "formatValue", format, missing(format), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("character", "list", "function"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "formatValue", fmtFuncArgs, missing(fmtFuncArgs), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$formatValue", "Formatting value...")
      if(is.null(value)) return(invisible(NULL))
      if(is.null(format)) return(base::as.character(value))
      clsv <- class(value)
      if(("numeric" %in% clsv)||("integer" %in% clsv)) {
        clsf <- class(format)
        if("character" %in% clsf) value <- sprintf(format, value)
        else if ("list" %in% clsf) {
          args <- format
          args$x <- value
          value <- do.call(base::format, args)
        }
        else if ("function" %in% class(format)) {
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
            args <- fmtFuncArgs
            args$x <- value
            value <- do.call(format, args)
          }
        }
        else value <- base::as.character(value)
      }
      else if("logical" %in% clsv) {
        clsf <- class(format)
        if("character" %in% clsf) {
          if (length(format)==2) {
            if(value==FALSE) value <- format[1]
            else if(value==TRUE) value <- format[2]
            else value <- "NA"
          }
          else if (length(format)==3) {
            if(value==FALSE) value <- format[1]
            else if(value==TRUE) value <- format[2]
            else value <- format[3]
          }
          else value <- sprintf(format, value)
        }
        else if ("list" %in% clsf) {
          args <- format
          args$x <- value
          value <- do.call(base::format, args)
        }
        else if ("function" %in% class(format)) {
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
            args <- fmtFuncArgs
            args$x <- value
            value <- do.call(format, args)
          }
        }
        else value <- base::as.character(value)
      }
      else if(("Date" %in% clsv)||("POSIXct" %in% clsv)||("POSIXlt" %in% clsv)) {
        clsf <- class(format)
        if ("character" %in% clsf) {
          if (format %in% c("%d","%i","%o","%x","%X")) value <- sprintf(format, value)
          else {
            args <- list(format)
            args$x <- value
            value <- do.call(base::format, args)
          }
        }
        else if ("list" %in% clsf) {
          args <- format
          args$x <- value
          value <- do.call(base::format, args)
        }
        else if ("function" %in% class(format)) {
          if (is.null(fmtFuncArgs)) value <- format(value)
          else {
            args <- fmtFuncArgs
            args$x <- value
            value <- do.call(format, args)
          }
        }
        else value <- base::as.character(value)
      }
      else value <- base::as.character(value)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$formatValue", "Formated value.")
      return(invisible(value))
    },

    #' @description
    #' Add a new named style to the table.
    #' @param styleName The name of the new style.
    #' @param declarations CSS style declarations in the form of a list, e.g.
    #' `list("font-weight"="bold", "color"="#0000FF")`
    #' @return The newly created `TableStyle` object.
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

    #' @description
    #' Create an inline style that can be used to override a base style.
    #' For general use cases, the `setStyling()` method provides a simpler
    #' and more direct way of styling specific parts of a table.
    #' @details
    #' Inline styles are typically used to override the style of some specific
    #' cells in a table.  Inline styles have no name.
    #' In HTML, they are rendered as 'style' attributes on specific table cells,
    #' where as named styles are linked to cells using the 'class' attribute.
    #' @param baseStyleName The name of an existing style to base the new style on.
    #' @param declarations CSS style declarations in the form of a list, e.g.
    #' `list("font-weight"="bold", "color"="#0000FF")`
    #' @return The newly created `TableStyle` object.
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

    #' @description
    #' Apply styling to a set of cells in the table.
    #' @details
    #' There are five ways to specify the part(s) of a table to apply
    #' styling to:\cr
    #' (1) By specifying a list of data groups using the `groups` argument.\cr
    #' (2) By specifying a list of cells using the `cells` argument.\cr
    #' (3) By specifying a single cell using the `rFrom` and `cFrom` arguments.\cr
    #' (4) By specifying a rectangular cell range using the `rFrom`, `cFrom`,
    #' `rTo` and `cTo` arguments.\cr
    #' (5) By specifying a vector of rowNumbers and/or columnNumbers.  If both
    #' rowNumbers and columnNumbers are specified, then the cells at the
    #' intersection of the specified row numbers and column numbers are styled.\cr
    #' If both rFrom/rTo and rowNumbers are specified, then rFrom/rTo constrain
    #' the row numbers specified in rowNumbers.\cr
    #' If both cFrom/cTo and columnNumbers are specified, then cFrom/cTo constrain
    #' the column numbers specified in columnNumbers.\cr
    #' See the "Styling" and "Finding and Formatting" vignettes for more
    #' information and many examples.
    #' @param rFrom An integer row number that specifies the start row for the
    #' styling changes.
    #' @param cFrom An integer column number that specifies the start column for the
    #' styling changes.
    #' @param rTo An integer row number that specifies the end row for the styling
    #' changes.
    #' @param cTo An integer column number that specifies the end column for the
    #' styling changes.
    #' @param rowNumbers An integer vector that specifies the row numbers for the
    #' styling changes.
    #' @param columnNumbers An integer vector that specifies the column numbers for
    #' the styling changes.
    #' @param cells A list containing `TableCell` objects.
    #' @param baseStyleName The name of a style to apply.
    #' @param style A `TableStyle` object to apply.
    #' @param declarations CSS style declarations to apply in the form of a list,
    #' e.g. `list("font-weight"="bold", "color"="#0000FF")`
    #' @return No return value.
    setStyling = function(rFrom=NULL, cFrom=NULL, rTo=NULL, cTo=NULL, rowNumbers=NULL, columnNumbers=NULL,
                          cells=NULL, baseStyleName=NULL, style=NULL, declarations=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", rFrom, missing(rFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", cFrom, missing(cFrom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", rTo, missing(rTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", cTo, missing(cTo), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("list", "TableCell"), allowedListElementClasses="TableCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "setStyling", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$setStyling", "Setting styling...")
      if(missing(baseStyleName)&&missing(style)&&missing(declarations)) { stop("BasicTable$setStyling():  Please specify at least one of baseStyleName, style or declarations.", call. = FALSE) }
      # style a cell or list of cells
      if(!is.null(cells)) {
        if("TableCell" %in% class(cells)) {
          cells <- list(cells)
        }
        if(length(cells)>0) {
          for(i in 1:length(cells)) {
            cell <- cells[[i]]
            if(!is.null(cell)) {
              if(!missing(baseStyleName)) { cell$baseStyleName <- baseStyleName }
              if(!missing(style)) { cell$style <- ifelse(is.null(style, NULL, style$getCopy())) }
              if((!missing(declarations))&&(!is.null(declarations))) {
                if (is.null(cell$style)) { cell$style <- TableStyle$new(parentTable=self, declarations=declarations) }
                else { cell$style$setPropertyValues(declarations) }
              }
            }
          }
        }
      }
      # styling cells by coordinates...
      styleCells <- FALSE
      rowCount <- self$rowCount
      columnCount <- self$columnCount
      ## switch to use the option legacy coordinates (this ignores the rowNumbers and columnNumbers parameters)
      if(isTRUE(private$p_compatibility$legacySetStylingRowColumnNumbers)) {
        if((!is.null(rFrom))&&(!is.null(cFrom))) {
          if(is.null(rTo)) rTo <- rFrom
          if(is.null(cTo)) cTo <- cFrom
          if(rTo<rFrom) { stop("BasicTable$setStyling():  rTo must be greater than or equal to rFrom.", call. = FALSE) }
          if(cTo<cFrom) { stop("BasicTable$setStyling():  cTo must be greater than or equal to cFrom.", call. = FALSE) }
          rowNumbers <- rFrom:rTo
          columnNumbers <- cFrom:cTo
          styleCells <- TRUE
        }
      }
      else {
        ## check if a single cell has been specified
        if((length(rowNumbers)==0)&&(length(columnNumbers)==0)&&
           (!is.null(rFrom))&&(!is.null(cFrom))&&(is.null(rTo))&&(is.null(cTo))) {
          rowNumbers <- rFrom
          columnNumbers <- cFrom
        }
        else
        {
          # specifying a range of cells
          if((length(rowNumbers)>0)||(!is.null(rFrom))||(!is.null(rTo))) {
            if(length(rowNumbers)==0) rowNumbers <- 1:max(rowCount, 1)
            if(!is.null(rFrom)) rowNumbers <- rowNumbers[rowNumbers >= min(rFrom)]
            if(!is.null(rTo)) rowNumbers <- rowNumbers[rowNumbers <= max(rTo)]
          }
          if((length(columnNumbers)>0)||(!is.null(cFrom))||(!is.null(cTo))) {
            if(length(columnNumbers)==0) columnNumbers <- 1:max(columnCount, 1)
            if(!is.null(cFrom)) columnNumbers <- columnNumbers[columnNumbers >= min(cFrom)]
            if(!is.null(cTo)) columnNumbers <- columnNumbers[columnNumbers <= max(cTo)]
          }
        }
        styleCells <- (length(rowNumbers)>0)||(length(columnNumbers)>0)
      }
      if(styleCells==TRUE) {
        if(is.null(private$p_cells)) stop("BasicTable$setStyling():  No cells exist in the table.", call. = FALSE)
        # set defaults for other axes
        if((length(rowNumbers)==0)&&(length(columnNumbers)>0)) rowNumbers <- 1:rowCount
        if((length(rowNumbers)>0)&&(length(columnNumbers)==0)) columnNumbers <- 1:columnCount
        # silently remove invalid row/column numbers
        if(min(rowNumbers)<1) rowNumbers <- rowNumbers[rowNumbers >= 1]
        if(max(rowNumbers)>rowCount) rowNumbers <- rowNumbers[rowNumbers <= rowCount]
        if(min(columnNumbers)<1) columnNumbers <- columnNumbers[columnNumbers >= 1]
        if(max(columnNumbers)>columnCount) columnNumbers <- columnNumbers[columnNumbers <= columnCount]
        # style cells
        for(r in rowNumbers) {
          for(c in columnNumbers) {
            cell <- self$cells$getCell(r, c)
            if(!is.null(cell)) {
              if(!missing(baseStyleName)) { cell$baseStyleName <- baseStyleName }
              if(!missing(style)) { cell$style <- ifelse(is.null(style), NULL, style$getCopy()) }
              if((!missing(declarations))&&(!is.null(declarations))) {
                if (is.null(cell$style)) { cell$style <- TableStyle$new(parentTable=self, declarations=declarations) }
                else { cell$style$setPropertyValues(declarations) }
              }
            }
          }
        }
      }

      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$setStyling", "Set styling.")
    },

    # mapType=value only allows fixed "from" values (either just a number or v==number): it maps discrete "from" values to discrete "to" values
    # mapType=range allows "from" as a number or as a range specified with "v": it matches values into "from" ranges, the "to" values are discrete
    # mapType=continuous only allows fixed "from" values (just a number or v==number): it rescales "from" numbers into continuous "to" values
    # allowed usage:
    # valueType=text, mapType=value:        0, "normal", 1, "bold", etc.
    # valueType=text, mapType=logic:        v==0, "normal", 1<v<=2, "bold", etc.
    # valueType=text, mapType=range:        0, "normal", 1, "bold", etc                styleLowerValues=FALSE, styleHigherValues=TRUE
    # valueType=number: supports the above, plus:
    # valueType=number, mapType=continuous: 0, 10, 1, 20, etc                          styleLowerValues=FALSE, styleHigherValues=TRUE
    # valueType=color:  supports the above, but the continuous option looks like:
    # valueType=color, mapType=continuous:  0, red, 1, yellow, etc                     styleLowerValues=FALSE, styleHigherValues=TRUE
    # Note in documentation that these methods are primarily for numerical data.  Parts may work for dates (e.g. mapType=value and range) ...
    # ... but other bits won't, e.g. v>=as.Date("2020-05-22"), or variables, e.g. v>=x where x is a local variable outside of the table (but the name could collide with a local variable inside the table function).

    #' @description
    #' Apply styling to table cells based on the value of each cell.
    #' @details
    #' `mapStyling()` is typically used to conditionally apply styling to cells
    #' based on the value of each individual cell, e.g. cells with values less
    #' than a specified number could be coloured red.\cr
    #' mapType="logic" maps values matching specified logical criteria to
    #' specific "to" values.  The logical criteria can be any of the following
    #' forms (the first matching mapping is used):\cr
    #' (1) a specific value, e.g. 12.\cr
    #' (2) a specific value equality condition, e.g. "v==12", where v
    #' represents the cell value.\cr
    #' (3) a value range expression using the following abbreviated form:
    #' "value1<=v<value2", e.g. "10<=v<15".  Only "<" or "<=" can be used
    #' in these value range expressions.\cr
    #' (4) a standard R logical expression, e.g.
    #' "10<=v && v<15".\cr
    #' Basic R functions that test the value can also be
    #' used, e.g. is.na(v).\cr
    #' See the "Styling" and Finding and Formatting" vignettes for more
    #' information and many examples.
    #' @param styleProperty The name of the style property to set on the specified
    #' cells, e.g. background-color.
    #' @param cells A list containing `TableCell` objects.
    #' @param valueType The type of style value to be set.  Must be one of:
    #' "text", "character", "number", "numeric", "color" or "colour".\cr
    #' "text" and "character" are equivalent.  "number" and "numeric" are equivalent.
    #' "color" and "colour" are equivalent.
    #' @param mapType The type of mapping to be performed.  The following mapping
    #' types are supported:\cr
    #' (1) "value" = a 1:1 mapping which maps each specified "from" value to the
    #' corresponding "to" value, e.g. 100 -> "green".\cr
    #' (2) "logic" = each from value is logical criteria.  See details.\cr
    #' (3) "range" = values between each pair of "from" values are mapped to the
    #' corresponding "to" value, e.g. values in the range 80-100 -> "green" (more
    #' specifically values greater than or equal to 80 and less than 100).\cr
    #' (4) "continuous" = rescales values between each pair of "from" values into
    #' the range of the corresponding pair of "to" values, e.g. if the "from" range
    #' is 80-100 and the corresponding "to" range is 0.8-1, then 90 -> 0.9.\cr
    #' "continuous" cannot be used with valueType="text"/"character".
    #' @param mappings The mappings to be applied, specified in one of the following
    #' three forms:\cr
    #' (1) a list containing pairs of values, e.g.
    #' `list(0, "red", 0.4, "yellow", 0.8, "green")`.\cr
    #' (2) a list containing "from" and "to" vectors/lists, e.g.
    #' `list(from=c(0, 0.4, 0.8), to=c("red", "yellow", "green"))`.\cr
    #' (3) a custom mapping function that will be invoked once per cell, e.g.
    #' `function(v, cell) { if(isTRUE(v>0.8)) return("green") }`.\cr
    #' Mappings must be specified in ascending order when valueType="range" or
    #' valueType="continuous".\cr
    #' If a custom mapping function is specified, then the valueType and mapType
    #' parameters are ignored.
    #' @param styleLowerValues A logical value, default `FALSE`, that specifies
    #' whether values less than the lowest specified "from" value should be styled
    #' using the style specified for the lowest "from" value.  Only applies when
    #' valueType="range" or valueType="continuous".
    #' @param styleHigherValues A logical value, default `TRUE`, that specifies
    #' whether values greater than the highest specified "from" value should be styled
    #' using the style specified for the highest "from" value.  Only applies when
    #' valueType="range" or valueType="continuous".
    #' @return No return value.
    mapStyling = function(styleProperty=NULL, cells=NULL, valueType="text", mapType="range", mappings=NULL, styleLowerValues=FALSE, styleHigherValues=TRUE) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", styleProperty, missing(styleProperty), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", cells, missing(cells), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("TableCell", "list"), allowedListElementClasses="TableCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", valueType, missing(valueType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("character", "text", "numeric", "number", "color", "colour"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", mapType, missing(mapType), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("value", "range", "logic", "continuous"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", mappings, missing(mappings), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("character", "list", "function"), allowedListElementClasses=c("character", "integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", styleLowerValues, missing(styleLowerValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "mapStyling", styleHigherValues, missing(styleHigherValues), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mapStyling", "Mapping styling...")
      # prep parameters
      if("TableCell" %in% class(cells)) cells <- list(cells)
      if(mapType=="numeric") mapType <- "number"
      if(mapType=="character") mapType <- "text"
      if("character" %in% class(mappings)) mappings <- as.list(mappings)
      if(valueType=="colour") valueType <- "color"
      # special case of a mapping function - this ignores the valueType and mapType arguments
      if("function" %in% class(mappings)) {
        for(cell in cells) {
          value <- cell$rawValue
          mappedValue <- mappings(value, cell)
          if((length(mappedValue)>0)&&(!is.na(mappedValue))) {
            declarations <- list()
            declarations[[styleProperty]] <- mappedValue
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
        if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mapStyling", "Mapped styling.")
        return(invisible())
      }
      # general or special case?
      if((length(mappings)==2)&&(length(intersect(names(mappings), c("from", "to")))==2)) {
        # special case of a two element list (from, to)
        if(length(intersect(class(mappings$from), c("character", "integer", "numeric", "list")))==0) {
          stop("BasicTable$mapStyling():  The 'from' values must be either character, integer, numeric or a list.", call. = FALSE)
        }
        # build the maps list
        maps <- list()
        mapCount <- min(length(mappings$from), length(mappings$to))
        m <- 1
        mMax <- mapCount
        for(m in 1:mMax) {
          if("list" %in% class(mappings$from)) vre <- mappings$from[[m]]
          else vre <- mappings$from[m]
          if("list" %in% class(mappings$to)) value <- mappings$to[[m]]
          else value <- mappings$to[m]
          nextVre <- NULL
          nextValue <- NULL
          if((m+1)<=mMax){
            if("list" %in% class(mappings$from)) nextVre <- mappings$from[[m+1]]
            else nextVre <- mappings$from[m+1]
            if("list" %in% class(mappings$to)) nextValue <- mappings$to[[m+1]]
            else nextValue <- mappings$to[m+1]
          }
          maps[[length(maps)+1]] <- list(vre=vre, value=value, nextVre=nextVre, nextValue=nextValue)
        }
      }
      else {
        # general case of a longer list of mappings
        # check some mappings, return if none
        if(length(mappings)<2) {
          if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mapStyling", "Mapped styling.")
          return(invisible())
        }
        # process the mappings into value pairs
        # every map has two values: vre (value range expression) = the "from" value, value = the "to" value
        # "from" here means the raw value (or range of raw values) that we are mapping from, i.e. the input of the mapping
        # "to" here means the result value, i.e. the output of the mapping
        # mapType=range and mapType=continuous also have: rangeStart, rangeEnd, where vre->rangeStart and nextVre->rangeEnd
        # valueType=color with mapType=continuous also has:
        maps <- list()
        mapCount <- length(mappings)
        m <- 1
        mMax <- mapCount
        while(m<mMax) {
          vre <- mappings[[m]]
          value <- NULL
          if((m+1)<=length(mappings)) value <- mappings[[m+1]]
          nextVre <- NULL
          if((m+2)<=length(mappings)) nextVre <- mappings[[m+2]]
          nextValue <- NULL
          if((m+3)<=length(mappings)) nextValue <- mappings[[m+3]]
          maps[[length(maps)+1]] <- list(vre=vre, value=value, nextVre=nextVre, nextValue=nextValue)
          # message("vre=", vre, " value=", value, " nextVre=", nextVre, " nextValue=", nextValue)
          m <- m+2
        }
      }
      # check/prepare maps
      if(mapType=="value") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          if((is.null(map$vre))||(length(map$vre)==0)) stop("BasicTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(vreIsSingleValue(map$vre)) {
            # write the single value back into the mapping to minimise repeated parsing
            maps[[m]]$vre <- vreGetSingleValue(map$vre)
          }
          else {
            stop(paste0("BasicTable$mapStyling():  The 'from' value for a mapping must be a single numerical value - invalid value ", map$vre), call. = FALSE)
          }
          if((is.null(map$value))||(length(map$value)==0)) stop("BasicTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
        }
      }
      else if(mapType=="logic") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          if((is.null(map$vre))||(length(map$vre)==0)) stop("BasicTable$mapStyling():  The 'from' criteria for a mapping cannot be null or blank.", call. = FALSE)
          if((is.null(map$value))||(length(map$value)==0)) stop("BasicTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
          testResult <- vreIsMatch(map$vre, 0, testOnly=TRUE)
        }
      }
      else if(mapType=="range") {
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          # basic map checks
          if((is.null(map$vre))||(length(map$vre)==0)) stop("BasicTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(!vreIsSingleValue(map$vre)) stop("BasicTable$mapStyling():  Only single-value 'from' values can be be used with a range mapping.", call. = FALSE)
          if(((is.null(map$value))||(length(map$value)==0))&&(m<length(maps))) {
            # the last value of a range mapping can be null, e.g. mappings=list(0, "red", 1000, "orange", 15000), note there is no colour for 15000
            stop("BasicTable$mapStyling():  The 'to' value for a mapping cannot be null or blank (except for the last specified 'from' value).", call. = FALSE)
          }
          # store the numerical range as part of the mapping so they don't need to be parsed repeatedly
          map$rangeStart <- vreGetSingleValue(map$vre)
          if((!is.null(map$nextVre))&&(vreIsSingleValue(map$nextVre))) map$rangeEnd <- vreGetSingleValue(map$nextVre)
          # check the "to" values
          if(valueType=="number") {
            if(!is.numeric(map$value)) stop(paste0("BasicTable$mapStyling():  The 'to' value '", map$value, "' must be numeric since valueType=number/numeric has been specified."), call. = FALSE)
          }
          else if(valueType=="color") {
            testColor <- parseColor(map$value)
            if(is.null(testColor)) stop(paste0("BasicTable$mapStyling():  The 'to' value '", map$value, "' must be a valid color/colour since valueType=color has been specified."), call. = FALSE)
          }
          # message(paste0("vre=", map$vre, " nextVre=", map$nextVre, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", map$value, " nextValue=", map$nextValue))
          maps[[m]] <- map
        }
        # check the order of the mappings
        fx <- function(x) { x$rangeStart }
        if(!isTRUE(all.equal(1:length(maps),order(sapply(maps, fx))))) {
          stop("BasicTable$mapStyling(): The 'from' values for range mappings must be specified in ascending order (lowest to highest).", call. = FALSE)
        }
      }
      else if(mapType=="continuous") {
        if(valueType=="text") stop("BasicTable$mapStyling():  Continuous mappings are not supported with text values.", call. = FALSE)
        for(m in 1:length(maps)) {
          map <- maps[[m]]
          # basic map checks
          if((is.null(map$vre))||(length(map$vre)==0)) stop("BasicTable$mapStyling():  The 'from' value for a mapping cannot be null or blank.", call. = FALSE)
          if(!vreIsSingleValue(map$vre)) stop("BasicTable$mapStyling():  Only single-value 'from' values can be be used with a continuous mapping.", call. = FALSE)
          if((is.null(map$value))||(length(map$value)==0)) stop("BasicTable$mapStyling():  The 'to' value for a mapping cannot be null or blank.", call. = FALSE)
          # store the numerical range as part of the mapping so they don't need to be parsed repeatedly
          map$rangeStart <- vreGetSingleValue(map$vre)
          if((!is.null(map$nextVre))&&(vreIsSingleValue(map$nextVre))) map$rangeEnd <- vreGetSingleValue(map$nextVre)
          # check the "to" values
          if(valueType=="number") {
            if(!is.numeric(map$value)) stop(paste0("BasicTable$mapStyling():  The 'to' value '", map$value, "' must be numeric since valueType=number/numeric has been specified."), call. = FALSE)
          }
          else if(valueType=="color") {
            map$startColorHex <- parseColor(map$value)
            if(is.null(map$startColorHex)) stop(paste0("BasicTable$mapStyling():  The 'to' value '", map$value, "' must be a valid color/colour since valueType=color/colour has been specified."), call. = FALSE)
            map$startColorList <- vreHexToClr(map$startColorHex)
            if(length(map$nextValue)>0) {
              map$endColorHex <- parseColor(map$nextValue)
              if(is.null(map$endColorHex)) stop(paste0("BasicTable$mapStyling():  The 'to' value '", map$nextValue, "' must be a valid color/colour since valueType=color/colour has been specified."), call. = FALSE)
              map$endColorList <- vreHexToClr(map$endColorHex)
            }
          }
          # message(paste0("vre=", map$vre, " nextVre=", map$nextVre, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", map$value, " nextValue=", map$nextValue))
          maps[[m]] <- map
        }
        # check the order of the mappings
        fx <- function(x) { x$rangeStart }
        if(!isTRUE(all.equal(1:length(maps),order(sapply(maps, fx))))) {
          stop("BasicTable$mapStyling(): The 'from' values for continuous mappings must be specified in ascending order (lowest to highest).", call. = FALSE)
        }
      }
      # map values
      mMax <- length(maps)
      if(mapType=="value") {
        for(cell in cells) {
          value <- cell$rawValue
          for(map in maps) {
            if(vreIsEqual(map$vre, value)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              break # jump to next cell
            }
          }
        }
      }
      else if(mapType=="logic") {
        for(cell in cells) {
          value <- cell$rawValue
          for(map in maps) {
            if(vreIsMatch(map$vre, value)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              break # jump to next cell
            }
          }
        }
      }
      else if(mapType=="range") {
        for(cell in cells) {
          cellStyleSet <- FALSE
          value <- cell$rawValue
          for(map in maps) {
            if(length(map$rangeStart)==0) next
            if(length(map$rangeEnd)==0) next
            if(length(map$value)==0) next
            if(isTRUE(map$rangeStart <= value && value < map$rangeEnd)) {
              declarations <- list()
              declarations[[styleProperty]] <- map$value
              self$setStyling(cells=cell, declarations=declarations)
              cellStyleSet <- TRUE
              break # jump to next cell
            }
          }
          # if this cell has been processed, jump to the next cell
          if(cellStyleSet==TRUE) next
          # none of the mappings have matched...
          # ...style the same as the first mapping?
          map <- maps[[1]]
          if(styleLowerValues && (length(map$rangeStart)>0) && isTRUE(value < map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$value
            self$setStyling(cells=cell, declarations=declarations)
          }
          # ...style the same as the last mapping?
          map <- maps[[mMax]]
          if(styleHigherValues && (length(map$rangeStart)>0) && isTRUE(value > map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$value
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
      }
      else if(mapType=="continuous") {
        for(cell in cells) {
          cellStyleSet <- FALSE
          value <- cell$rawValue
          for(map in maps) {
            if(length(map$rangeStart)==0) next
            if(length(map$rangeEnd)==0) next
            if(length(map$value)==0) next
            if(length(map$nextValue)==0) next
            if(isTRUE(map$rangeStart <= value && value < map$rangeEnd)) {
              if(valueType=="number") {
                value <- vreScaleNumber(map$value, map$nextValue, map$rangeStart, map$rangeEnd, value)
                cellStyleSet <- TRUE
              }
              else if(valueType=="color") {
                # message(paste0("startColorHex=", map$startColorHex, " endColorHex=", map$endColorHex, " rangeStart=", map$rangeStart, " rangeEnd=", map$rangeEnd, " value=", value))
                value <- vreScale2Colours(map$startColorList, map$endColorList, map$rangeStart, map$rangeEnd, value)
                cellStyleSet <- TRUE
              }
              if((length(value)>0)&&(!is.na(value))) {
                declarations <- list()
                declarations[[styleProperty]] <- value
                self$setStyling(cells=cell, declarations=declarations)
                break # jump to next cell
              }
            }
          }
          # if this cell has been processed, jump to the next cell
          if(cellStyleSet==TRUE) next
          # none of the mappings have matched...
          # ...style the same as the first mapping?
          map <- maps[[1]]
          if(styleLowerValues && (length(map$rangeStart)>0) && isTRUE(value < map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$startColorHex
            self$setStyling(cells=cell, declarations=declarations)
          }
          # ...style the same as the last mapping?
          map <- maps[[mMax]]
          if(styleHigherValues && (length(map$rangeStart)>0) && isTRUE(value > map$rangeStart) && (length(map$value)>0)) {
            declarations <- list()
            declarations[[styleProperty]] <- map$startColorHex
            self$setStyling(cells=cell, declarations=declarations)
          }
        }
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$mapStyling", "Mapped styling.")
      return(invisible())
    },

    #' @description
    #' Clear the cells of the table.
    #' @details
    #' The cells are reset automatically when structural changes are made to the
    #' table, so this method often doesn't needs to be called explicitly.
    #' @return No return value.
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
    #' @param specifyCellsAsList Specify how cells are retrieved.
    #' Default `TRUE`. More information is provided in the details section.
    #' @param rowNumbers A vector of row numbers that specify the rows or
    #' cells to retrieve.
    #' @param columnNumbers A vector of column numbers that specify the columns
    #' or cells to retrieve.
    #' @param cellCoordinates A list of two-element vectors that specify the
    #' coordinates of cells to retrieve.  Ignored when `specifyCellsAsList=FALSE`.
    #' @param excludeEmptyCells Default `FALSE`.  Specify `TRUE` to exclude empty
    #' cells.
    #' @param matchMode Either "simple" (default) or "combinations":\cr
    #' "simple" specifies that row and column arguments are considered separately
    #' (logical OR), e.g. rowNumbers=1 and columnNumbers=2 will match all cells in
    #' row 1 and all cells in column 2.\cr
    #' "combinations" specifies that row and column arguments are considered together
    #' (logical AND), e.g. rowNumbers=1 and columnNumbers=2 will match only the
    #' cell single at location (1, 2).\cr
    #' Arguments `rowNumbers` and `columnNumbers` are
    #' affected by the match mode.  All other arguments are not.
    #' @return A list of `TableCell` objects.
    getCells = function(specifyCellsAsList=TRUE, rowNumbers=NULL, columnNumbers=NULL, cellCoordinates=NULL, excludeEmptyCells=FALSE, matchMode="simple") {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", specifyCellsAsList, missing(specifyCellsAsList), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", excludeEmptyCells, missing(excludeEmptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "getCells", matchMode, missing(matchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCells", "Getting cells...")
      if(is.null(private$p_cells)) stop("BasicTable$getCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$getCells(specifyCellsAsList=specifyCellsAsList, rowNumbers=rowNumbers, columnNumbers=columnNumbers,
                                        cellCoordinates=cellCoordinates, excludeEmptyCells=excludeEmptyCells, matchMode=matchMode)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$getCells", "Got cells.")
      return(invisible(cells))
    },

    #' @description
    #' Find cells matching specified criteria.
    #' See the "Finding and Formatting" vignette for graphical examples.
    #' @details
    #' The valueRanges parameter can be any of the following
    #' forms:\cr
    #' (1) a specific value, e.g. 12.\cr
    #' (2) a specific value equality condition, e.g. "v==12", where v
    #' represents the cell value.\cr
    #' (3) a value range expression using the following abbreviated form:
    #' "value1<=v<value2", e.g. "10<=v<15".  Only "<" or "<=" can be used
    #' in these value range expressions.\cr
    #' (4) a standard R logical expression, e.g.
    #' "10<=v && v<15".\cr
    #' Basic R functions that test the value can also be
    #' used, e.g. is.na(v).\cr
    #' @param minValue A numerical value specifying a minimum value threshold.
    #' @param maxValue A numerical value specifying a maximum value threshold.
    #' @param exactValues A vector or list specifying a set of allowed values.
    #' @param includeNull specify TRUE to include `NULL` in the matched cells,
    #' FALSE to exclude `NULL` values.
    #' @param includeNA specify TRUE to include `NA` in the matched cells,
    #' FALSE to exclude `NA` values.
    #' @param valueRanges A vector specifying one or more value range expressions which
    #' the cell values must match.  If multiple value range expressions are specified,
    #' then the cell value must match any of one the specified expressions.  See details.
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
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", minValue, missing(minValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", maxValue, missing(maxValue), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", exactValues, missing(exactValues), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", listElementsMustBeAtomic=TRUE)
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", valueRanges, missing(valueRanges), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", includeNull, missing(includeNull), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", includeNA, missing(includeNA), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", emptyCells, missing(emptyCells), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("include", "exclude", "only"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", rowNumbers, missing(rowNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", columnNumbers, missing(columnNumbers), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", cellCoordinates, missing(cellCoordinates), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("integer", "numeric"))
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", cells, missing(cells), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("TableCell", "list"), allowedListElementClasses="TableCell")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "findCells", rowColumnMatchMode, missing(rowColumnMatchMode), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("simple", "combinations"))
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$findCells", "Finding cells...")
      if(is.null(private$p_cells)) stop("BasicTable$findCells():  No cells exist to retrieve.", call. = FALSE)
      cells <- private$p_cells$findCells(minValue=minValue, maxValue=maxValue, exactValues=exactValues, valueRanges=valueRanges,
                                         includeNull=includeNull, includeNA=includeNA, emptyCells=emptyCells,
                                         rowNumbers=rowNumbers, columnNumbers=columnNumbers,
                                         cellCoordinates=cellCoordinates, cells=cells, rowColumnMatchMode=rowColumnMatchMode)
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$findCells", "Found cells.")
      return(invisible(cells))
    },

    #' @description
    #' Outputs a plain text representation of the table to the console or
    #' returns a character representation of the table.
    #' @param asCharacter `FALSE` (default) outputs to the console, specify `TRUE`
    #' to instead return a character value (does not output to console).
    #' @return Plain text representation of the table.
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

    #' @description
    #' Convert the table to a matrix, with or without headings.
    #' @details
    #' See the "Outputs" vignette for a comparison of outputs.
    #' @param firstRowAsColumnNames `TRUE` to use the first row of the table as
    #'   the column names in the matrix.  Default value `FALSE`.
    #' @param firstColumnAsRowNames `TRUE` to use the first column of the table
    #'   as the row names in the matrix.  Default value `FALSE`.
    #' @param rawValue `FALSE` (default) outputs the formatted (character) values.
    #' Specify `TRUE` to output the raw cell values.
    #' @return A matrix.
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

    #' @description
    #' Convert the table to a data frame, with or without headings.
    #' @details
    #' See the "Outputs" vignette for a comparison of outputs.
    #' @param firstRowAsColumnNames `TRUE` to use the first row of the table as
    #'   the column names in the data frame  Default value `FALSE`.
    #' @param firstColumnAsRowNames `TRUE` to use the first column of the table
    #'   as the row names in the data frame.  Default value `FALSE`.
    #' @param rawValue `FALSE` (default) outputs the formatted (character) values.
    #' Specify `TRUE` to output the raw cell values.
    #' @param stringsAsFactors Specify `TRUE` to convert strings to factors,
    #' default is `default.stringsAsFactors()` for R < 4.1.0 and `FALSE`
    #' for R >= 4.1.0.
    #' @return A matrix.
    asDataFrame = function(firstRowAsColumnNames=FALSE, firstColumnAsRowNames=FALSE, rawValue=FALSE, stringsAsFactors=NULL) {
      if(private$p_argumentCheckMode > 0) {
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", firstRowAsColumnNames, missing(firstRowAsColumnNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", firstColumnAsRowNames, missing(firstColumnAsRowNames), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", rawValue, missing(rawValue), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "asDataFrame", stringsAsFactors, missing(stringsAsFactors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asDataFrame", "Getting table as a data frame...",
                    list(includeHeaders=includeHeaders, rawValue=rawValue))
      if(is.null(private$p_cells)) stop("BasicTable$asDataFrame():  No cells exist to retrieve.", call. = FALSE)
      # stringsAsFactors default depends on the version of R...
      if(is.null(stringsAsFactors)) {
        if(getRversion() < "4.0.0") {
          # old version, retain existing behaviour with no warning
          stringsAsFactors <- default.stringsAsFactors()
        }
        else if (getRversion() < "4.1.0") {
          stringsAsFactors <- default.stringsAsFactors()
          # generate a warning if the default is TRUE as this will change to FALSE in future
          if(stringsAsFactors) {
            warning("BasicTable$asTidyDataFrame(): In a future version of R, default.stringsAsFactors() will be deprecated and removed, at which time the 'stringsAsFactors' argument will default to FALSE.  Explictly set the 'stringsAsFactors' argument to remove this warning.")
          }
        }
        else {
          # default to FALSE for R 4.1.0 onwards
          stringsAsFactors <- FALSE
        }
      }
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
      df <- as.data.frame(dfColumns, col.names=columnNames, stringsAsFactors=stringsAsFactors)
      if(!is.null(rowNames)) rownames(df) <- rowNames
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asDataFrame", "Got table as a data frame.")
      return(df)
    },

    #' @description
    #' Get the CSS declarations for the table.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple tables exist.
    #' @return A character value containing the CSS style declaration.
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

    #' @description
    #' Generate a HTML representation of the table.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple tables exist.
    #' @return A list containing HTML tags from the `htmltools` package.
    #' Convert this to a character variable using `as.character()`.
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

    #' @description
    #' Save a HTML representation of the table to file.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param filePath The file to save the HTML to.
    #' @param fullPageHTML `TRUE` (default) includes basic HTML around the
    #' table HTML so that the result file is a valid HTML file.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple tables exist.
    #' @return No return value.
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

    #' @description
    #' Render a HTML representation of the table as an HTML widget.
    #' @details
    #' See the "Outputs" vignette for more details and examples.
    #' @param width The width of the widget.
    #' @param height The height of the widget.
    #' @param styleNamePrefix A character variable specifying a prefix for all named
    #' CSS styles, to avoid style name collisions where multiple tables exist.
    #' @return A HTML widget from the `htmlwidgets` package.
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

    #' @description
    #' Write the table into the specified workbook and worksheet at
    #' the specified row-column location.
    #' @details
    #' See the Excel Output vignette for more details.
    #' @param wb A `Workbook` object representing the Excel file being written
    #' to.
    #' @param wsName A character value specifying the name of the worksheet to
    #' write to.
    #' @param topRowNumber An integer value specifying the row number in the
    #' Excel worksheet to write the table.
    #' @param leftMostColumnNumber An integer value specifying the column number
    #' in the Excel worksheet to write the table.
    #' @param outputValuesAs Must be one of "rawValue" (default),
    #' "formattedValueAsText" or "formattedValueAsNumber" to specify
    #' how cell values are written into the Excel sheet.
    #' @param applyStyles Default `TRUE` to write styling information to the cell.
    #' @param mapStylesFromCSS Default `TRUE` to automatically convert CSS style
    #' declarations to their Excel equivalents.
    #' @return No return value.
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

    #' @description
    #' Capture a call for tracing purposes.
    #' This is an internal method.
    #' @param methodName The name of the method being invoked.
    #' @param desc Short description of method call.
    #' @param detailList A list containing detail such as parameter values.
    #' @return No return value.
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

    #' @description
    #' Return the contents of the table as a list for debugging.
    #' @return A list of various object properties..
    asList = function() {
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asList", "Getting list...")
      lst <- list(
        type = "BasicTable"
      )
      if(!is.null(private$p_cells)) lst$cells <- private$p_cells$asList()
      if(private$p_traceEnabled==TRUE) self$trace("BasicTable$asList", "Got list.")
      return(lst)
    },

    #' @description
    #' Return the contents of the table as JSON for debugging.
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
    },

    #' @description
    #' Use the `listviewer` package to view the table as JSON for debugging.
    #' @return No return value.
    viewJSON = function() {
      if (!requireNamespace("listviewer", quietly = TRUE)) {
        stop("BasicTable$asJSON():  The listviewer package is needed to view the internal structure of the BasicTable as JSON.  Please install it.", call. = FALSE)
      }
      listviewer::jsonedit(self$asList(), mode="code")
    },

    #' @description
    #' Clean-up the table.
    #' @return No return value.
    finalize = function() {
      if(!is.null(private$p_traceFile)) close(private$p_traceFile)
    }
  ),
  active = list(

    #' @field argumentCheckMode The level of argument checking to perform.
    #' One of "auto", "none", "minimal", "basic", "balanced" (default)
    #' or "full".
    argumentCheckMode = function(value) { return(private$p_argumentCheckMode) },

    #' @field compatibility A list containing compatibility options to force
    #' legacy behaviours.  See the NEWS file for details.
    compatibility = function(value) { return(private$p_compatibility) },

    #' @field traceEnabled A logical value indicating whether actions are logged to
    #'   a trace file.
    traceEnabled = function(value){
      if(missing(value)) return(invisible(private$p_traceEnabled))
      else {
        if(is.logical(value)) private$p_traceEnabled <- value
        else stop("BasicTable$traceEnabled: value must be logical (TRUE, FALSE, T, F)", call. = FALSE)
        return(invisible())
      }
    },

    #' @field cells A `TableCells` object containing all of the cells in the body of
    #'   the table.
    cells = function(value) { return(invisible(private$p_cells)) },

    #' @field allCells A list of all of the cells in the table, where each element
    #' in the list is a 'TableCell' object.
    allCells = function(value) {
      if(is.null(private$p_cells)) stop("BasicTable$allCells:  No cells exist.", call. = FALSE)
      return(invisible(private$p_cells$all))
    },

    #' @field mergedCells A `TableCellRanges` object describing the merged cells.
    mergedCells = function(value) { return(invisible(private$p_mergedCells)) },

    #' @field rowCount The number of rows in the table.
    rowCount = function(value) { return(invisible(private$p_cells$rowCount)) },

    #' @field columnCount The number of columns in the table.
    columnCount = function(value) { return(invisible(private$p_cells$columnCount)) },

    #' @field asCharacter The plain-text representation of the table.
    asCharacter = function() { return(self$print(asCharacter=TRUE)) },

    #' @field theme The name of the theme used to style the table.
    #' If setting this property, either a theme name can be used, or
    #' a list can be used (which specifies a simple theme) or a
    #' `TableStyles` object can be used.
    #' See the "Styling" vignette for details and examples.
    theme = function(value) {
      if(missing(value)) {
        if(is.null(private$p_styles)) return(invisible(NULL))
        else return(invisible(private$p_styles$theme))
      }
      else {
        if(private$p_argumentCheckMode > 0) {
          checkArgument(private$p_argumentCheckMode, TRUE, "BasicTable", "theme", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "list", "TableStyles"), allowedListElementClasses="character")
        }
        if("character" %in% class(value)) private$p_styles <- getTblTheme(parentTable=self, themeName=value)
        else if("list" %in% class(value)) private$p_styles <- getSimpleColoredTblTheme(parentTable=self, theme=value)
        else if("TableStyles" %in% class(value)) private$p_styles <- value
        return(invisible())
      }
    },

    #' @field styles A `TableStyles` object containing the styles used to theme the
    #'   table.
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

    #' @field allowExternalStyles Default `FALSE`, which means the `TableStyles`
    #' object checks that style names specified for styling the different
    #' parts of the table must exist in the styles collection.  If they do
    #' not an error will occur.  Specify `TRUE` to disable this check, e.g. if
    #' the style definitions are not managed by `basictabler` but instead
    #' in an external system.
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

    #' @field allTimings The time taken for various activities related to
    #'   constructing the table.
    allTimings = function(value) {
      descriptions <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$desc), NA, x$desc)) })
      user <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["user.self"]), NA, x$time["user.self"])) })
      system <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["sys.self"]), NA, x$time["sys.self"])) })
      elapsed <- sapply(private$p_timings, function(x) { return(ifelse(is.null(x$time["elapsed"]), NA, x$time["elapsed"])) })
      return(data.frame(action=descriptions, user=user, system=system, elapsed=elapsed))
    },

    #' @field significantTimings The time taken for various activities related to
    #'   constructing the table, where the elapsed time > 0.1 seconds.
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
    p_lastInstanceId = NULL,
    p_data = NULL,
    p_styles = NULL,
    p_cells = NULL,
    p_mergedCells = NULL,
    p_htmlRenderer = NULL,
    p_openxlsxRenderer = NULL,
    p_compatibility = NULL,
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
