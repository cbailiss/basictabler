#' R6 class that converts a table to a flextable from the `flextabler` package.
#'
#' @description
#' The `TableFlexTblRenderer` class creates a representation of a table using
#' the `flextable` package.  See the Output vignette for more details.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should not be used by end users.  It is an internal class
#' # created only by the BasicTable class.  It is used when converting to a
#' # flextable.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' library(flextable)
#' flextbl <- tbl$asFlexTable()

TableFlexTblRenderer <- R6::R6Class("TableFlexTblRenderer",
  public = list(

    #' @description
    #' Create a new `TableFlexTblRenderer` object.
    #' @param parentTable Owning table.
    #' @return No return value.
    initialize = function(parentTable) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$new", "Creating new FlexTbl Renderer...")
      private$p_styles <- TableFlexTblStyles$new(parentTable=private$p_parentTable)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$new", "Created new FlexTbl Renderer.")
    },

    #' @description
    #' Write a value to a cell, optionally with styling and cell merging.
    #' @param ft The flextable to write to.
    #' @param rowNumber The row number of the cell where the value is to be
    #'   written.
    #' @param columnNumber The column number of the cell where the value is to be
    #'   written.
    #' @param value The value to be written.  Since the flextable is created
    #'   from a data frame, this argument can be omitted.
    #' @param applyStyles `TRUE` (default) to also set the styling of the cell,
    #'   `FALSE` to only write the value.
    #' @param baseStyleName The name of the style from the table theme to apply
    #'   to the cell.
    #' @param style A `TableStyle` object that contains additional styling to
    #'   apply to the cell.
    #' @param mapFromCss `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding Excel styles, `FALSE` to apply only the specified xl
    #'   styles.
    #' @param mergeRows If the cell is to be merged with adjacent cells, then an
    #'   integer or numeric vector specifying the row numbers of the merged
    #'   cell.  NULL (default) to not merge cells.
    #' @param mergeColumns If the cell is to be merged with adjacent cells, then
    #'   an integer or numeric vector specifying the column numbers of the
    #'   merged cell.  NULL (default) to not merge cells.
    #' @return The updated flextable definition.
    writeToCell = function(ft=NULL, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL) {
       if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", rowNumber, missing(rowNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", columnNumber, missing(columnNumber), allowMissing=TRUE, allowNull=FALSE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", value, missing(value), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("logical", "integer", "numeric", "character", "Date", "POSIXct", "POSIXlt"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", mergeRows, missing(mergeRows), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "writeToCell", mergeColumns, missing(mergeColumns), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("integer", "numeric"))
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$writeToWorksheet", "Writing to cell...")
      # write the value
      if(!missing(value)) ft <- flextable::compose(ft, i=rowNumber, j=columnNumber, value=flextable::as_paragraph(value))
      # merge cells
      isMergedCells <- isNumericValue(mergeRows)&&isNumericValue(mergeColumns)
      if(isMergedCells) ft <- flextable::merge_at(ft, i=mergeRows, j=mergeColumns)
      # styling
      if(applyStyles) {
        ftStyle <- NULL
        # just a base style (these were all already added to the FlexTblStyles collection, so can just do a find based on the name only)
        if(isTextValue(baseStyleName)&&is.null(style)) {
          ftStyle <- private$p_styles$findNamedStyle(baseStyleName)
          if(is.null(ftStyle)) stop(paste0("TableFlexTblRenderer$writeToWorksheet(): Unable to find named style '", baseStyleName, "'."), call. = FALSE)
          # if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          # else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
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
          ftStyle <- private$p_styles$findOrAddStyle(action="findOrAdd", baseStyleName=baseStyleName, isBaseStyle=FALSE, style=fullStyle, mapFromCss=mapFromCss)
          if(is.null(ftStyle)) stop("TableFlexTblRenderer$writeToWorksheet(): Failed to find or add style.", call. = FALSE)
          #if(isMergedCells) openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=mergeRows, cols=mergeColumns, gridExpand=TRUE)
          #else openxlsx::addStyle(wb, sheet=wsName, style=openxlsxStyle$openxlsxStyle, rows=rowNumber, cols=columnNumber, gridExpand=TRUE)
        }
        # style range
        styleRows <- ifelse(isMergedCells, mergeRows, rowNumber)
        styleCols <- ifelse(isMergedCells, mergeColumns, columnNumber)
        # font name
        if(isTextValue(ftStyle$fontName)) ft <- flextable::font(ft, i=styleRows, j=styleCols, fontname=ftStyle$fontName)
        # font size
        if(!is.null(ftStyle$fontSize)) ft <- flextable::fontsize(ft, i=styleRows, j=styleCols, size=ftStyle$fontSize)
        # bold
        if(isTRUE(ftStyle$bold)) ft <- flextable::bold(ft, i=styleRows, j=styleCols, bold=TRUE)
        # italic
        if(isTRUE(ftStyle$italic)) ft <- flextable::italic(ft, i=styleRows, j=styleCols, italic=TRUE)
        # background colour
        if(isTextValue(ftStyle$bgColor)) ft <- flextable::bg(ft, i=styleRows, j=styleCols, bg=ftStyle$bgColor)
        # text colour
        if(isTextValue(ftStyle$textColor)) ft <- flextable::color(ft, i=styleRows, j=styleCols, color=ftStyle$textColor)
        # hAlign
        if(isTextValue(ftStyle$hAlign)) ft <- flextable::align(ft, i=styleRows, j=styleCols, align=ftStyle$hAlign)
        # vAlign
        if(isTextValue(ftStyle$vAlign)) ft <- flextable::valign(ft, i=styleRows, j=styleCols, valign=ftStyle$vAlign)
        # textRotation
        if(isTextValue(ftStyle$textRotation)) ft <- flextable::rotate(ft, i=styleRows, j=styleCols, rotation=ftStyle$textRotation)
        # padding
        paddingSet <- FALSE
        paddingSet <- paddingSet || !is.null(ftStyle$paddingAll)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingLeft)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingRight)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingTop)
        paddingSet <- paddingSet || !is.null(ftStyle$paddingBottom)
        # TODO - the message below shows that when the padding is specified in the table theme in CSS as padding="2px 8px 2px 2px",
        # then the padding is not being processed properly into the paddingLeft, paddingRight, paddingTop and paddingBottom.
        # Need to add additional logic to TableFlexTblStyles$findOrAddStyle to handle this.
        message(paste(ftStyle$paddingAll, ftStyle$paddingLeft, ftStyle$paddingRight, ftStyle$paddingTop, ftStyle$paddingBottom))
        if(paddingSet==TRUE) ft <- flextable::padding(ft, i=styleRows, j=styleCols, padding=ftStyle$paddingAll,
                                                padding.left=ftStyle$paddingLeft, padding.right=ftStyle$paddingRight,
                                                padding.top=ftStyle$paddingTop, padding.bottom=ftStyle$paddingBottom)
        # borders
        # TODO - need to use hline and vline functions to set borders.

        # TODO - rename the files (and class names) to FlexTableStyle, FlexTableStyles and FlexTableRenderer.

        # TODO - testing all of the properties when using setStyling, including setting both standard CSS styles and the ft styles.

        # TODO - add flextable output to the outputs vignettes of both basictabler and pivottabler.

        # TODO - update the readme to mention flextable (so allowing output to Word and PPT - include these in the outputs vignette for both basictabler and pivottabler).

        # TODO - update the styling reference with details of the mappings between CSS and ft styles.

        # TODO - update NEWS file.

        # TODO - devtools::document()
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$writeToWorksheet", "Written to cell.")
      return(ft)
    },

    #' @description
    #' Convert table to a flextable table.
    #' @param applyStyles `TRUE` (default) to also set the styling of the cells,
    #'   `FALSE` to only write the value.
    #' @param mapStylesFromCSS `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding flextable styles where possible, `FALSE` to apply only the
    #'   specified ft styles.
    #' @return No return value.
    asFlexTable = function(applyStyles=TRUE, mapStylesFromCSS=TRUE) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "asFlexTable", applyStyles, missing(applyStyles), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableFlexTblRenderer", "asFlexTable", mapStylesFromCSS, missing(mapStylesFromCSS), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$asFlexTable", "Converting to flextable...")

      # clear the styles
      private$p_styles$clearStyles()

      # create an FlexTblStyle for each named style in the table
      if(applyStyles) private$p_styles$addNamedStyles(mapFromCss=mapStylesFromCSS)

      # output the styles
      # message(private$p_styles$asString(seperator="\r\n\r\n"))

      # do the export, first creating a basic flextable table from a data frame of formatted values, then going cell by cell to apply the styling/formatting
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

      # special case of no rows and no columns, return a blank empty table
      if((rowCount==0)&&(columnCount==0)) {
        df <- data.frame(data="(no data)")
        ft <- flextable::flextable(df)
        ft <- flextable::autofit(ft)
        ft <- flextable::delete_part(ft, part="header")
        ft <- flextable::delete_part(ft, part="footer")
        ft <- self$writeToCell(ft, rowNumber=NULL, columnNumber=NULL, value=NULL, applyStyles=TRUE, baseStyleName=NULL, style=NULL, mapFromCss=TRUE, mergeRows=NULL, mergeColumns=NULL)
        return(invisible(ft))
      }

      # update the merged cell info
      private$p_parentTable$applyCellMerges()

      # create the flextable (based on formatted values, since flextables store the data as dataframes, so don't support different data types in the same column)
      ft <- flextable::flextable(bt$asDataFrame(rawValue=FALSE))
      ft <- flextable::autofit(ft)
      ft <- flextable::delete_part(ft, part="header")
      ft <- flextable::delete_part(ft, part="footer")

      # render the rows
      for(r in 1:rowCount) {
        # render the cell values
        for(c in 1:columnCount) {

          # get cell
          cell <- private$p_parentTable$cells$getCell(r, c)

          # if a merged cell and not the root of the merge, then skip to next cell
          if(cell$isMerged && (!cell$isMergeRoot)) { next }

          # merge cell if needed
          mergeRows <- NULL
          mergeColumns <- NULL
          if(cell$isMerged) {
            mergeRange <- private$p_parentTable$mergedCells$ranges[[cell$mergeIndex]]
            mergeRows <- r:(r + mergeRange$rSpan - 1)
            mergeColumns <- c:(c + mergeRange$cSpan - 1)
          }

          # get style info
          if(cell$cellType=="root") cs <- rootStyle
          else if(cell$cellType=="rowHeader") cs <- rowHeaderStyle
          else if(cell$cellType=="columnHeader") cs <- colHeaderStyle
          else if(cell$cellType=="total") cs <- totalStyle
          else cs <- cellStyle
          if(!is.null(cell$baseStyleName)) cs <- cell$baseStyleName

          # write cell (value is omitted since it was set in the data frame used to create the flex table)
          ft <- self$writeToCell(ft, rowNumber=r, columnNumber=c, applyStyles=applyStyles,
                                 baseStyleName=cs, style=cell$style, mapFromCss=mapStylesFromCSS,
                                 mergeRows=mergeRows, mergeColumns=mergeColumns)
        }
      }

      # TableFlexTblStyles collection builds up a collection of styles. This
      # follows the same pattern used by the OpenXlsx renderer - even though
      # the flextable package does not use a collection of styles (same
      # approach taken so that both renderers are consistent).
      # The styles are used as follows: Before rendering
      # begins, a TableFlexTblStyle object is created for each named style
      # defined in the table. As each cell is rendered to the workbook, if
      # a cell with a named style but no other style overrides is
      # encountered, then these styles are used.  If a cell is encountered
      # with overrides, then the styles collection is searched to find the
      # matching style.  If not found, then a new style is added which can be
      # reused later. The TableFlexTblStyle object looks for styles named
      # "ft-".  If found, these are used.  These closely map to the formatting
      # options available in the flextable package.
      # If style settings named "ft-" are not found, then an attempt is made
      # to use the CSS equivalents.  Currently, the following still needs
      # doing in terms of style settings:
      #
      # * font-name, font-size, etc have been mapped, but the general CSS font
      # setting (e.g. font: 15px arial, sans-serif;) has not been mapped.
      # * border settings have been partially mapped.  border, border-top,
      # border-bottom, etc have been mapped.  The very specific (e.g.
      # border-bottom-width, border-bottom-style, etc) have not been mapped.

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblRenderer$asFlexTable", "Converted to flextable.")
      return(ft)
    }
  ),
  private = list(
    p_parentTable = NULL,
    p_styles = NULL
  )
)
