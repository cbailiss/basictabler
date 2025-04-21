#' R6 class that specifies Excel styling as used by the openxlsx package.
#'
#' @description
#' The `TableOpenXlsxStyle` class specifies the styling for cells in an
#' Excel worksheet.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link[R6]{R6Class}} object.
#' @examples
#' # This class should only be created by using the functions in the table.
#' # It is not intended to be created by users outside of the table.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' library(openxlsx)
#' wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
#' addWorksheet(wb, "Data")
#' tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#'                          topRowNumber=1, leftMostColumnNumber=1,
#'                          applyStyles=TRUE, mapStylesFromCSS=TRUE)
#' # Use saveWorkbook() to save the Excel file.

TableOpenXlsxStyle <- R6::R6Class("TableOpenXlsxStyle",
  public = list(

    #' @description
    #' Create a new `TableOpenXlsxStyle` object.
    #' @param parentTable Owning table.
    #' @param baseStyleName The name of the base style in the table.
    #' @param isBaseStyle `TRUE` when this style is the equivalent of a named style in
    #'   the table, `FALSE` if this style has additional settings over and above
    #'   the base style of the same name.
    #' @param fontName The name of the font (single font name, not a CSS style
    #'   list).
    #' @param fontSize The size of the font (units: point).
    #' @param bold `TRUE` if text is bold.
    #' @param italic `TRUE` if text is italic.
    #' @param underline `TRUE` if text is underlined.
    #' @param strikethrough `TRUE` if text has a line through it.
    #' @param superscript `TRUE` if text is small and raised.
    #' @param subscript `TRUE` if text is small and lowered.
    #' @param fillColor The background colour for the cell (as a hex value, e.g.
    #'   #00FF00).
    #' @param textColor The color of the text (as a hex value).
    #' @param hAlign The horizontal alignment of the text:  left, center or right.
    #' @param vAlign The vertical alignment of the text:  top, middle or bottom.
    #' @param wrapText `TRUE` if the text is allowed to wrap onto multiple lines.
    #' @param textRotation The rotation angle of the text or 255 for vertical.
    #' @param indent The text indentation.
    #' @param borderAll A list (with elements style and color) specifying the border
    #'   settings for all four sides of each cell at once.
    #' @param borderLeft A list (with elements style and color) specifying the
    #'   border settings for the left border of each cell.
    #' @param borderRight A list (with elements style and color) specifying the
    #'   border settings for the right border of each cell.
    #' @param borderTop A list (with elements style and color) specifying the border
    #'   settings for the top border of each cell.
    #' @param borderBottom A list (with elements style and color) specifying the
    #'   border settings for the bottom border of each cell.
    #' @param valueFormat The Excel formatting applied to the field value.  One
    #'   of the following values: "GENERAL", "NUMBER", "CURRENCY", "ACCOUNTING",
    #'   "DATE", "LONGDATE", TIME, "PERCENTAGE", "FRACTION", "SCIENTIFIC",
    #'   "TEXT", "COMMA". Or for dates/datetimes, a combination of d, m, y. Or
    #'   for numeric values, use 0.00 etc.
    #' @param minColumnWidth The minimum width of this column.
    #' @param minRowHeight The minimum height of this row.
    #' @return No return value.
    initialize = function(parentTable, baseStyleName=NULL, isBaseStyle=NULL,
                         fontName=NULL, fontSize=NULL, bold=NULL,
                         italic=NULL, underline=NULL, strikethrough=NULL,
                         superscript=NULL, subscript=NULL, fillColor=NULL,
                         textColor=NULL, hAlign=NULL, vAlign=NULL, wrapText=NULL,
                         textRotation=NULL, indent=NULL,
                         borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                         borderTop=NULL, borderBottom=NULL, valueFormat=NULL,
                         minColumnWidth=NULL, minRowHeight=NULL) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", baseStyleName, missing(baseStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", isBaseStyle, missing(isBaseStyle), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", fontName, missing(fontName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", fontSize, missing(fontSize), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=4, maxValue=72)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", bold, missing(bold), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", italic, missing(italic), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", underline, missing(underline), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", strikethrough, missing(strikethrough), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", superscript, missing(superscript), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", subscript, missing(subscript), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", fillColor, missing(fillColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", textColor, missing(textColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", hAlign, missing(hAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("left", "center", "right"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", vAlign, missing(vAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("top", "middle", "bottom"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", wrapText, missing(wrapText), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", textRotation, missing(textRotation), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", indent, missing(indent), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=500)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", borderAll, missing(borderAll), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", borderLeft, missing(borderLeft), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", borderRight, missing(borderRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", borderTop, missing(borderTop), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", borderBottom, missing(borderBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", valueFormat, missing(valueFormat), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", minColumnWidth, missing(minColumnWidth), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=255)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableOpenXlsxStyle", "initialize", minRowHeight, missing(minRowHeight), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=0, maxValue=400)
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxStyle$new", "Creating new Table Style...", list())

     if(!is.null(fillColor)) {
       check <- grep("#[0-9A-F]{6}", fillColor)
       if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  fillColor must be in the format #NNNNNN.", call. = FALSE)
     }
     if(!is.null(textColor)) {
       check <- grep("#[0-9A-F]{6}", textColor)
       if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  textColor must be in the format #NNNNNN.", call. = FALSE)
     }

     allowedStyles <- c("none", "thin", "medium", "dashed", "dotted", "thick", "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot")
     if(!is.null(borderAll)) {
       borderStyle <- borderAll[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableOpenXlsxStyle$initialize():  borderAll$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderAll[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  borderAll$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderLeft)) {
       borderStyle <- borderLeft[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableOpenXlsxStyle$initialize():  borderLeft$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderLeft[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  borderLeft$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderRight)) {
       borderStyle <- borderRight[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableOpenXlsxStyle$initialize():  borderRight$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderRight[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  borderRight$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderTop)) {
       borderStyle <- borderTop[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableOpenXlsxStyle$initialize():  borderTop$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderTop[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  borderTop$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }
     if(!is.null(borderBottom)) {
       borderStyle <- borderBottom[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableOpenXlsxStyle$initialize():  borderBottom$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderBottom[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableOpenXlsxStyle$initialize():  borderAll$color must be in the format #NNNNNN.", call. = FALSE)
       }
     }

     private$p_baseStyleName <- baseStyleName
     private$p_isBaseStyle <- isBaseStyle
     private$p_fontName <- fontName
     private$p_fontSize <- fontSize
     private$p_bold <- bold
     private$p_italic <- italic
     private$p_underline <- underline
     private$p_strikethrough <- strikethrough
     private$p_superscript <- superscript
     private$p_subscript <- subscript
     private$p_fillColor <- fillColor
     private$p_textColor <- textColor
     private$p_hAlign <- hAlign
     private$p_vAlign <- vAlign
     private$p_wrapText <- wrapText
     private$p_textRotation <- textRotation
     private$p_indent <- indent
     private$p_borderAll <- borderAll
     private$p_borderLeft <- borderLeft
     private$p_borderRight <- borderRight
     private$p_borderTop <- borderTop
     private$p_borderBottom <- borderBottom
     private$p_valueFormat <- valueFormat
     private$p_minColumnWidth <- minColumnWidth
     private$p_minRowHeight <- minRowHeight

     self$createOpenXslxStyle()

     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableOpenXlsxStyle$new", "Created new Table Style")
   },

   #' @description
   #' Check if this style matches the specified base style name.
   #' @param baseStyleName The style name to compare to.
   #' @return `TRUE` if the style name matches, `FALSE` otherwise.
   isBasicStyleNameMatch = function(baseStyleName=NULL) {
     return(private$p_isBaseStyle && (baseStyleName==private$p_baseStyleName))
   },

   #' @description
   #' Check if this style matches the specified style properties.
   #' @param baseStyleName The style name to compare to.
   #' @param isBaseStyle Whether the style being compared to is a base style.
   #' @param fontName The font name to compare to.
   #' @param fontSize The font size to compare to.
   #' @param bold The style property bold to compare to.
   #' @param italic The style property italic to compare to.
   #' @param underline The style property underline to compare to.
   #' @param strikethrough The style property strikethrough to compare to.
   #' @param superscript The style property superscript to compare to.
   #' @param subscript The style property subscript to compare to.
   #' @param fillColor The style property fillColor to compare to.
   #' @param textColor The style property textColor to compare to.
   #' @param hAlign The style property hAlign to compare to.
   #' @param vAlign The style property vAlign to compare to.
   #' @param wrapText The style property wrapText to compare to.
   #' @param textRotation The style property textRotation to compare to.
   #' @param indent The style property indent to compare to.
   #' @param borderAll The style property borderAll to compare to.
   #' @param borderLeft The style property borderLeft to compare to.
   #' @param borderRight The style property borderRight to compare to.
   #' @param borderTop The style property borderTop to compare to.
   #' @param borderBottom The style property borderBottom to compare to.
   #' @param valueFormat The style value format to compare to.
   #' @param minColumnWidth The style property minColumnWidth to compare to.
   #' @param minRowHeight The style property minRowHeight to compare to.
   #' @return `TRUE` if the style matches, `FALSE` otherwise.
   isFullStyleDetailMatch = function(baseStyleName=NULL, isBaseStyle=NULL,
                           fontName=NULL, fontSize=NULL, bold=NULL,
                           italic=NULL, underline=NULL, strikethrough=NULL,
                           superscript=NULL, subscript=NULL, fillColor=NULL,
                           textColor=NULL, hAlign=NULL, vAlign=NULL, wrapText=NULL,
                           textRotation=NULL, indent=NULL,
                           borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                           borderTop=NULL, borderBottom=NULL,
                           valueFormat=NULL,
                           minColumnWidth=NULL, minRowHeight=NULL) {
      if(isBaseStyle && private$p_isBaseStyle) {
        # if this is a base style and the style we are trying to find a match for is also a base style
        # (i.e. with no other additional style settings applied over the top) then just compare the names
        return(private$isMatch(baseStyleName, private$p_baseStyleName))
      }
      else {
        # message(paste0("Match to ", private$p_baseStyleName, " isBaseStyle=", private$p_isBaseStyle))
        # message(paste0("fontName: ", private$isMatch(fontName, private$p_fontName)))
        # message(paste0("fontSize: ", private$isMatch(fontSize, private$p_fontSize)))
        # message(paste0("bold: ", private$isMatch(bold, private$p_bold)))
        # message(paste0("italic: ", private$isMatch(italic, private$p_italic)))
        # message(paste0("underline: ", private$isMatch(underline, private$p_underline)))
        # message(paste0("strikethrough: ", private$isMatch(strikethrough, private$p_strikethrough)))
        # message(paste0("superscript: ", private$isMatch(superscript, private$p_superscript)))
        # message(paste0("subscript: ", private$isMatch(subscript, private$p_subscript)))
        # message(paste0("fillColor: ", private$isMatch(fillColor, private$p_fillColor)))
        # message(paste0("textColor: ", private$isMatch(textColor, private$p_textColor)))
        # message(paste0("hAlign: ", private$isMatch(hAlign, private$p_hAlign)))
        # message(paste0("vAlign: ", private$isMatch(vAlign, private$p_vAlign)))
        # message(paste0("wrapText: ", private$isMatch(wrapText, private$p_wrapText)))
        # message(paste0("textRotation: ", private$isMatch(textRotation, private$p_textRotation)))
        # message(paste0("indent: ", private$isMatch(indent, private$p_indent)))
        # message(paste0("borderAll: ", private$isBorderMatch(borderAll, private$p_borderAll)))
        # message(paste0("borderLeft: ", private$isBorderMatch(borderLeft, private$p_borderLeft)))
        # message(paste0("borderRight: ", private$isBorderMatch(borderRight, private$p_borderRight)))
        # message(paste0("borderTop: ", private$isBorderMatch(borderTop, private$p_borderTop)))
        # message(paste0("borderBottom: ", private$isBorderMatch(borderBottom, private$p_borderBottom)))
        # message(paste0("valueFormat: ", private$isMatch(valueFormat, private$p_valueFormat)))
        # message(paste0("minColumnWidth: ", private$isMatch(minColumnWidth, private$p_minColumnWidth)))
        # message(paste0("minRowHeight: ", private$isMatch(minRowHeight, private$p_minRowHeight)))
        # message("")
        return(private$isMatch(fontName, private$p_fontName) && private$isMatch(fontSize, private$p_fontSize) &&
                private$isMatch(bold, private$p_bold) && private$isMatch(italic, private$p_italic) &&
                private$isMatch(underline, private$p_underline) && private$isMatch(strikethrough, private$p_strikethrough) &&
                private$isMatch(superscript, private$p_superscript) && private$isMatch(subscript, private$p_subscript) &&
                private$isMatch(fillColor, private$p_fillColor) && private$isMatch(textColor, private$p_textColor) &&
                private$isMatch(hAlign, private$p_hAlign) && private$isMatch(vAlign, private$p_vAlign) &&
                private$isMatch(wrapText, private$p_wrapText) && private$isMatch(textRotation, private$p_textRotation) &&
                private$isMatch(indent, private$p_indent) &&
                private$isBorderMatch(borderAll, private$p_borderAll) &&
                private$isBorderMatch(borderLeft, private$p_borderLeft) && private$isBorderMatch(borderRight, private$p_borderRight) &&
                private$isBorderMatch(borderTop, private$p_borderTop) && private$isBorderMatch(borderBottom, private$p_borderBottom) &&
                private$isMatch(valueFormat, private$p_valueFormat) &&
                private$isMatch(minColumnWidth, private$p_minColumnWidth) && private$isMatch(minRowHeight, private$p_minRowHeight))
      }
    },

   #' @description
   #' Create the `openxlsx` style based on the specified style properties.
   #' @return No return value.
    createOpenXslxStyle = function() {
      # consolidate the borders
      borderSides <- list()
      borderColors <- list()
      borderStyles <- list()
      if((!is.null(private$p_borderAll)) &&
         (isTextValue(private$p_borderAll[["style"]]))){
        borderSides <- list("left", "right", "top", "bottom")
        if(isTextValue(private$p_borderAll[["color"]])) {
          clr <- private$p_borderAll[["color"]]
        }
        else clr <- "#000000"
        borderColors <- list(clr, clr, clr, clr)
        stl <- private$p_borderAll[["style"]]
        borderStyles <- list(stl, stl, stl, stl)
      }
      else {
        if((!is.null(private$p_borderLeft)) &&
           (isTextValue(private$p_borderLeft[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "left"
          if(isTextValue(private$p_borderLeft[["color"]])) {
            clr <- private$p_borderLeft[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderLeft[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderRight)) &&
           (isTextValue(private$p_borderRight[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "right"
          if(isTextValue(private$p_borderRight[["color"]])) {
            clr <- private$p_borderRight[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderRight[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderTop)) &&
           (isTextValue(private$p_borderTop[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "top"
          if(isTextValue(private$p_borderTop[["color"]])) {
            clr <- private$p_borderTop[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderTop[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
        if((!is.null(private$p_borderBottom)) &&
           (isTextValue(private$p_borderBottom[["style"]]))) {
          borderSides[[length(borderSides)+1]] <- "bottom"
          if(isTextValue(private$p_borderBottom[["color"]])) {
            clr <- private$p_borderBottom[["color"]]
          }
          else clr <- "#000000"
          borderColors[[length(borderColors)+1]] <- clr
          stl <- private$p_borderBottom[["style"]]
          borderStyles[[length(borderStyles)+1]] <- stl
        }
      }
      borderSides <- unlist(borderSides)
      borderColors <- unlist(borderColors)
      borderStyles <- unlist(borderStyles)

      # consolidate the text decoration
      textDecoration <- list()
      if(private$p_bold) textDecoration[[length(textDecoration)+1]] <- "bold"
      if(private$p_italic) textDecoration[[length(textDecoration)+1]] <- "italic"
      if(private$p_underline) textDecoration[[length(textDecoration)+1]] <- "underline"
      if(private$p_strikethrough) textDecoration[[length(textDecoration)+1]] <- "strikeout"
      textDecoration <- unlist(textDecoration)

      # other values
      valueFormat <- private$p_valueFormat
      if(!isTextValue(valueFormat)) valueFormat <- "GENERAL"
      vAlign <- private$p_vAlign
      if(isTextValue(vAlign)&&(vAlign=="middle")) vAlign <- "center"

      # message(private$p_vAlign)
      # message(class(private$p_vAlign))
      # message(paste0("borderSides= ", paste(borderSides, collapse=",")))
      # message(paste0("borderColors= ", paste(borderColors, collapse=",")))
      # message(paste0("borderStyles= ", paste(borderStyles, collapse=",")))

      # create the style
      private$p_openxlsxStyle <- openxlsx::createStyle(
        fontName=private$p_fontName, fontSize=private$p_fontSize,
        fontColour=private$p_textColor, numFmt=valueFormat,
        border=borderSides, borderColour=borderColors, borderStyle=borderStyles,
        fgFill=private$p_fillColor, halign=private$p_hAlign, valign=vAlign,
        textDecoration=textDecoration, wrapText=private$p_wrapText,
        textRotation=private$p_textRotation, indent=private$p_indent)
    },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       baseStyleName = private$p_baseStyleName,
       isBaseStyle = private$p_isBaseStyle, # TRUE if this style is equivalent to the base style of the same name
       fontName = private$p_fontName,
       fontSize = private$p_fontSize,
       bold = private$p_bold,
       italic = private$p_italic,
       underline = private$p_underline,
       strikethrough = private$p_strikethrough,
       superscript = private$p_superscript,
       subscript = private$p_subscript,
       fillColor = private$p_fillColor,
       textColor = private$p_textColor,
       hAlign = private$p_hAlign,
       vAlign = private$p_vAlign,
       wrapText = private$p_wrapText,
       textRotation = private$p_textRotation,
       indent = private$p_indent,
       borderAll = private$p_borderAll,
       borderLeft = private$p_borderLeft,
       borderRight = private$p_borderRight,
       borderTop = private$p_borderTop,
       borderBottom = private$p_borderBottom,
       valueFormat = private$p_valueFormat,
       minColumnWidth = private$p_minColumnWidth,
       minRowHeight = private$p_minRowHeight
     )
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
   },

   #' @description
   #' Return the contents of this object as a string for debugging.
   #' @return A character representation of various object properties.
   asString = function() {
     lst <- self$asList()
     if(is.null(lst)||length(lst)==0) return("")
     nms <- names(lst)
     getNvp <- function(i) {
       v <- lst[[i]]
       if((!is.null(v))&&(length(v)>1)) {
         v <- paste0("(", paste(v, collapse=", "), ")")
       }
       paste0(nms[i], "=", v)
     }
     nvp <- sapply(1:length(lst), getNvp)
     return(invisible(paste0("{ ", paste(nvp, collapse=", "), " }")))
   }
  ),
  active = list(

    #' @field baseStyleName The name of the base style in the table.
    baseStyleName = function(value) { return(invisible(private$p_baseStyleName)) },

    #' @field isBaseStyle `TRUE` when this style is the equivalent of a named style in
    #'   the table, `FALSE` if this style has additional settings over and above
    #'   the base style of the same name.
    isBaseStyle = function(value) { return(invisible(private$p_isBaseStyle)) },

    #' @field fontName The name of the font (single font name, not a CSS style
    #'   list).
    fontName = function(value) { return(invisible(private$p_fontName)) },

    #' @field fontSize The size of the font (units: point).
    fontSize = function(value) { return(invisible(private$p_fontSize)) },

    #' @field bold TRUE` if text is bold.
    bold = function(value) { return(invisible(private$p_bold)) },

    #' @field italic `TRUE` if text is italic.
    italic = function(value) { return(invisible(private$p_italic)) },

    #' @field underline `TRUE` if text is underlined.
    underline = function(value) { return(invisible(private$p_underline)) },

    #' @field strikethrough `TRUE` if text has a line through it.
    strikethrough = function(value) { return(invisible(private$p_strikethrough)) },

    #' @field superscript `TRUE` if text is small and raised.
    superscript = function(value) { return(invisible(private$p_superscript)) },

    #' @field subscript `TRUE` if text is small and lowered.
    subscript = function(value) { return(invisible(private$p_subscript)) },

    #' @field fillColor The background colour for the cell (as a hex value, e.g.
    #'   #00FF00).
    fillColor = function(value) { return(invisible(private$p_fillColor)) },

    #' @field textColor The color of the text (as a hex value).
    textColor = function(value) { return(invisible(private$p_textColor)) },

    #' @field hAlign The horizontal alignment of the text:  left, center or right.
    hAlign = function(value) { return(invisible(private$p_hAlign)) },

    #' @field vAlign The vertical alignment of the text:  top, middle or bottom.
    vAlign = function(value) { return(invisible(private$p_vAlign)) },

    #' @field wrapText TRUE if the text is allowed to wrap onto multiple lines.
    wrapText = function(value) { return(invisible(private$p_wrapText)) },

    #' @field textRotation The rotation angle of the text or 255 for vertical.
    textRotation = function(value) { return(invisible(private$p_textRotation)) },

    #' @field indent The text indentation.
    indent = function(value) { return(invisible(private$p_indent)) },

    #' @field borderAll A list (with elements style and color) specifying the border
    #'   settings for all four sides of each cell at once.
    borderAll = function(value) { return(invisible(private$p_borderAll)) },

    #' @field borderLeft A list (with elements style and color) specifying the
    #'   border settings for the left border of each cell.
    borderLeft = function(value) { return(invisible(private$p_borderLeft)) },

    #' @field borderRight A list (with elements style and color) specifying the
    #'   border settings for the right border of each cell.
    borderRight = function(value) { return(invisible(private$p_borderRight)) },

    #' @field borderTop A list (with elements style and color) specifying the border
    #'   settings for the top border of each cell.
    borderTop = function(value) { return(invisible(private$p_borderTop)) },

    #' @field borderBottom A list (with elements style and color) specifying the
    #'   border settings for the bottom border of each cell.
    borderBottom = function(value) { return(invisible(private$p_borderBottom)) },

    #' @field valueFormat  The Excel formatting applied to the field value.  One
    #'   of the following values: "GENERAL", "NUMBER", "CURRENCY", "ACCOUNTING",
    #'   "DATE", "LONGDATE", TIME, "PERCENTAGE", "FRACTION", "SCIENTIFIC",
    #'   "TEXT", "COMMA". Or for dates/datetimes, a combination of d, m, y. Or
    #'   for numeric values, use 0.00 etc.
    valueFormat = function(value) { return(invisible(private$p_valueFormat)) },

    #' @field minColumnWidth The minimum width of this column.
    minColumnWidth = function(value) { return(invisible(private$p_minColumnWidth)) },

    #' @field minRowHeight The minimum height of this row.
    minRowHeight = function(value) { return(invisible(private$p_minRowHeight)) },

    #' @field openxlsxStyle The return value from openxlsx::createStyle().
    openxlsxStyle = function(value) { return(invisible(private$p_openxlsxStyle)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_baseStyleName = NULL,
    p_isBaseStyle = NULL, # TRUE if this style is equivalent to the base style of the same name
    p_fontName = NULL,
    p_fontSize = NULL,
    p_bold = NULL,
    p_italic = NULL,
    p_underline = NULL,
    p_strikethrough = NULL,
    p_superscript = NULL,
    p_subscript = NULL,
    p_fillColor = NULL,
    p_textColor = NULL,
    p_hAlign = NULL,
    p_vAlign = NULL,
    p_wrapText = NULL,
    p_textRotation = NULL,
    p_indent = NULL,
    p_borderAll = NULL,
    p_borderLeft = NULL,
    p_borderRight = NULL,
    p_borderTop = NULL,
    p_borderBottom = NULL,
    p_valueFormat = NULL,
    p_minColumnWidth = NULL,
    p_minRowHeight = NULL,
    isMatch = function(value1, value2) {
      if(is.null(value1) && is.null(value2)) return(TRUE)
      if(is.null(value1)) return(FALSE)
      if(is.null(value2)) return(FALSE)
      if(is.na(value1) && is.na(value2)) return(TRUE)
      if(is.na(value1)) return(FALSE)
      if(is.na(value2)) return(FALSE)
      if(length(value1) != length(value2)) return(FALSE)
      return(value1==value2)
    },
    isBorderMatch = function(border1, border2) {
      if(is.null(border1) && is.null(border2)) return(TRUE)
      return(private$isMatch(border1[["style"]], border2[["style"]]) &&
             private$isMatch(border1[["color"]], border2[["color"]]))
    },
    p_openxlsxStyle = NULL
  )
)




