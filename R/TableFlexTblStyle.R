#' R6 class that specifies styling as used by the `flextable` package.
#'
#' @description
#' The `TableFlexTblStyle` class specifies the styling for cells in a
#' table from the flextable package.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should only be created by using the functions in the table.
#' # It is not intended to be created by users outside of the table.
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' library(flextable)
#' flextbl <- tbl$asFlexTable()

TableFlexTblStyle <- R6::R6Class("TableFlexTblStyle",
  public = list(

    #' @description
    #' Create a new `TableFlexTblStyle` object.
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
    #' @param bgColor The background colour for the cell (as a hex value, e.g.
    #'   #00FF00).
    #' @param textColor The color of the text (as a hex value).
    #' @param hAlign The horizontal alignment of the text:  left, center or right.
    #' @param vAlign The vertical alignment of the text:  top, middle or bottom.
    #' @param textRotation The rotation angle of the text or 255 for vertical.
    #' @param paddingAll The padding to apply to all sides of each cell.
    #' @param paddingLeft The padding to apply to the left side of each cell.
    #' @param paddingRight The padding to apply to the right side of each cell.
    #' @param paddingTop The padding to apply to the top of each cell.
    #' @param paddingBottom The padding to apply to the bottom of each cell.
    #' @param borderAll A list (with elements style, color and width) specifying
    #'   the border settings for all four sides of each cell at once.
    #' @param borderLeft A list (with elements style, color and width)
    #'   specifying the border settings for the left border of each cell.
    #' @param borderRight A list (with elements style, color and width)
    #'   specifying the border settings for the right border of each cell.
    #' @param borderTop A list (with elements style, color and width) specifying
    #'   the border settings for the top border of each cell.
    #' @param borderBottom A list (with elements style, color and width)
    #'   specifying the border settings for the bottom border of each cell.
    #' @return No return value.
    initialize = function(parentTable, baseStyleName=NULL, isBaseStyle=NULL,
                         fontName=NULL, fontSize=NULL, bold=NULL,
                         italic=NULL, bgColor=NULL, textColor=NULL,
                         hAlign=NULL, vAlign=NULL, textRotation=NULL,
                         paddingAll=NULL, paddingLeft=NULL, paddingRight=NULL,
                         paddingTop=NULL, paddingBottom=NULL,
                         borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                         borderTop=NULL, borderBottom=NULL) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", baseStyleName, missing(baseStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", isBaseStyle, missing(isBaseStyle), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", fontName, missing(fontName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", fontSize, missing(fontSize), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"), minValue=4, maxValue=72)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", bold, missing(bold), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", italic, missing(italic), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", bgColor, missing(bgColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", textColor, missing(textColor), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", maxLength=7)
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", hAlign, missing(hAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("left", "center", "right"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", vAlign, missing(vAlign), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character", allowedValues=c("top", "middle", "bottom"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", textRotation, missing(textRotation), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", paddingAll, missing(paddingAll), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", paddingLeft, missing(paddingLeft), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", paddingRight, missing(paddingRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", paddingTop, missing(paddingTop), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", paddingBottom, missing(paddingBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses=c("numeric", "integer"))
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", borderAll, missing(borderAll), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", borderLeft, missing(borderLeft), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", borderRight, missing(borderRight), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", borderTop, missing(borderTop), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableFlexTblStyle", "initialize", borderBottom, missing(borderBottom), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list")
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblStyle$new", "Creating new Table Style...", list())

     if(!is.null(bgColor)) {
       check <- grep("#[0-9A-F]{6}", bgColor)
       if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  bgColor must be in the format #NNNNNN.", call. = FALSE)
     }
     if(!is.null(textColor)) {
       check <- grep("#[0-9A-F]{6}", textColor)
       if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  textColor must be in the format #NNNNNN.", call. = FALSE)
     }

     allowedStyles <- c("none", "solid", "dotted", "dashed")
     if(!is.null(borderAll)) {
       borderStyle <- borderAll[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableFlexTblStyle$initialize():  borderAll$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderAll[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  borderAll$color must be in the format #NNNNNN.", call. = FALSE)
       }
       borderWidth <- borderAll[["width"]]
       if((!is.null(borderWidth)) && is.numeric(borderWidth)) {
         check <- suppressWarnings(as.numeric(borderWidth))
         if(is.na(check)||(check<0)) stop("TableFlexTblStyle$initialize():  borderAll$width must be a number >= 0.", call. = FALSE)
       }
     }
     if(!is.null(borderLeft)) {
       borderStyle <- borderLeft[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableFlexTblStyle$initialize():  borderLeft$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderLeft[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  borderLeft$color must be in the format #NNNNNN.", call. = FALSE)
       }
       borderWidth <- borderLeft[["width"]]
       if((!is.null(borderWidth)) && is.numeric(borderWidth)) {
         check <- suppressWarnings(as.numeric(borderWidth))
         if(is.na(check)||(check<0)) stop("TableFlexTblStyle$initialize():  borderLeft$width must be a number >= 0.", call. = FALSE)
       }
     }
     if(!is.null(borderRight)) {
       borderStyle <- borderRight[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableFlexTblStyle$initialize():  borderRight$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderRight[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  borderRight$color must be in the format #NNNNNN.", call. = FALSE)
       }
       borderWidth <- borderRight[["width"]]
       if((!is.null(borderWidth)) && is.numeric(borderWidth)) {
         check <- suppressWarnings(as.numeric(borderWidth))
         if(is.na(check)||(check<0)) stop("TableFlexTblStyle$initialize():  borderRight$width must be a number >= 0.", call. = FALSE)
       }
     }
     if(!is.null(borderTop)) {
       borderStyle <- borderTop[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableFlexTblStyle$initialize():  borderTop$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderTop[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  borderTop$color must be in the format #NNNNNN.", call. = FALSE)
       }
       borderWidth <- borderTop[["width"]]
       if((!is.null(borderWidth)) && is.numeric(borderWidth)) {
         check <- suppressWarnings(as.numeric(borderWidth))
         if(is.na(check)||(check<0)) stop("TableFlexTblStyle$initialize():  borderTop$width must be a number >= 0.", call. = FALSE)
       }
     }
     if(!is.null(borderBottom)) {
       borderStyle <- borderBottom[["style"]]
       if(!is.null(borderStyle)) {
         if(!(borderStyle %in% allowedStyles)) {
           stop(paste0("TableFlexTblStyle$initialize():  borderBottom$style must be one of the following values: ",
                       paste(allowedStyles, collapse=", ")), call. = FALSE)
         }
       }
       borderColor <- borderBottom[["color"]]
       if(!is.null(borderColor)) {
         check <- grep("#[0-9A-F]{6}", borderColor)
         if((length(check)==0)||(check==FALSE)) stop("TableFlexTblStyle$initialize():  borderBottom$color must be in the format #NNNNNN.", call. = FALSE)
       }
       borderWidth <- borderBottom[["width"]]
       if((!is.null(borderWidth)) && is.numeric(borderWidth)) {
         check <- suppressWarnings(as.numeric(borderWidth))
         if(is.na(check)||(check<0)) stop("TableFlexTblStyle$initialize():  borderBottom$width must be a number >= 0.", call. = FALSE)
       }
     }

     private$p_baseStyleName <- baseStyleName
     private$p_isBaseStyle <- isBaseStyle
     private$p_fontName <- fontName
     private$p_fontSize <- fontSize
     private$p_bold <- bold
     private$p_italic <- italic
     private$p_bgColor <- bgColor
     private$p_textColor <- textColor
     private$p_hAlign <- hAlign
     private$p_vAlign <- vAlign
     private$p_textRotation <- textRotation
     private$p_paddingAll <- paddingAll
     private$p_paddingLeft <- paddingLeft
     private$p_paddingRight <- paddingRight
     private$p_paddingTop <- paddingTop
     private$p_paddingBottom <- paddingBottom
     private$p_borderAll <- borderAll
     private$p_borderLeft <- borderLeft
     private$p_borderRight <- borderRight
     private$p_borderTop <- borderTop
     private$p_borderBottom <- borderBottom

     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableFlexTblStyle$new", "Created new Table Style")
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
   #' @param bgColor The style property bgColor to compare to.
   #' @param textColor The style property textColor to compare to.
   #' @param hAlign The style property hAlign to compare to.
   #' @param vAlign The style property vAlign to compare to.
   #' @param textRotation The style property textRotation to compare to.
   #' @param paddingAll The padding to apply to all sides of each cell.
   #' @param paddingLeft The padding to apply to the left side of each cell.
   #' @param paddingRight The padding to apply to the right side of each cell.
   #' @param paddingTop The padding to apply to the top of each cell.
   #' @param paddingBottom The padding to apply to the bottom of each cell.
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
                                     italic=NULL, bgColor=NULL, textColor=NULL,
                                     hAlign=NULL, vAlign=NULL, textRotation=NULL,
                                     paddingAll=NULL, paddingLeft=NULL, paddingRight=NULL,
                                     paddingTop=NULL, paddingBottom=NULL,
                                     borderAll=NULL, borderLeft=NULL, borderRight=NULL,
                                     borderTop=NULL, borderBottom=NULL) {
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
       # message(paste0("bgColor: ", private$isMatch(bgColor, private$p_bgColor)))
       # message(paste0("textColor: ", private$isMatch(textColor, private$p_textColor)))
       # message(paste0("hAlign: ", private$isMatch(hAlign, private$p_hAlign)))
       # message(paste0("vAlign: ", private$isMatch(vAlign, private$p_vAlign)))
       # message(paste0("textRotation: ", private$isMatch(textRotation, private$p_textRotation)))
       # message(paste0("paddingAll: ", private$isBorderMatch(paddingAll, private$p_paddingAll)))
       # message(paste0("paddingLeft: ", private$isBorderMatch(paddingLeft, private$p_paddingLeftLeft)))
       # message(paste0("paddingRight: ", private$isBorderMatch(paddingRight, private$p_paddingRight)))
       # message(paste0("paddingTop: ", private$isBorderMatch(paddingTop, private$p_bpaddingTop)))
       # message(paste0("paddingBottom: ", private$isBorderMatch(paddingBottom, private$p_paddingBottom)))
       # message(paste0("borderAll: ", private$isBorderMatch(borderAll, private$p_borderAll)))
       # message(paste0("borderLeft: ", private$isBorderMatch(borderLeft, private$p_borderLeft)))
       # message(paste0("borderRight: ", private$isBorderMatch(borderRight, private$p_borderRight)))
       # message(paste0("borderTop: ", private$isBorderMatch(borderTop, private$p_borderTop)))
       # message(paste0("borderBottom: ", private$isBorderMatch(borderBottom, private$p_borderBottom)))
       # message("")
       return(private$isMatch(fontName, private$p_fontName) && private$isMatch(fontSize, private$p_fontSize) &&
                private$isMatch(bold, private$p_bold) && private$isMatch(italic, private$p_italic) &&
                private$isMatch(bgColor, private$p_bgColor) && private$isMatch(textColor, private$p_textColor) &&
                private$isMatch(hAlign, private$p_hAlign) && private$isMatch(vAlign, private$p_vAlign) &&
                private$isMatch(textRotation, private$p_textRotation) &&
                private$isMatch(paddingAll, private$p_paddingAll) &&
                private$isMatch(paddingLeft, private$p_paddingLeft) && private$isMatch(paddingRight, private$p_paddingRight) &&
                private$isMatch(paddingTop, private$p_paddingTop) && private$isMatch(paddingBottom, private$p_paddingBottom) &&
                private$isBorderMatch(borderAll, private$p_borderAll) &&
                private$isBorderMatch(borderLeft, private$p_borderLeft) && private$isBorderMatch(borderRight, private$p_borderRight) &&
                private$isBorderMatch(borderTop, private$p_borderTop) && private$isBorderMatch(borderBottom, private$p_borderBottom))
     }
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
       bgColor = private$p_bgColor,
       textColor = private$p_textColor,
       hAlign = private$p_hAlign,
       vAlign = private$p_vAlign,
       textRotation = private$p_textRotation,
       paddingAll = private$p_paddingAll,
       paddingLeft = private$p_paddingLeft,
       paddingRight = private$p_paddingRight,
       paddingTop = private$p_paddingTop,
       paddingBottom = private$p_paddingBottom,
       borderAll = private$p_borderAll,
       borderLeft = private$p_borderLeft,
       borderRight = private$p_borderRight,
       borderTop = private$p_borderTop,
       borderBottom = private$p_borderBottom
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

    #' @field bgColor The background colour for the cell (as a hex value, e.g.
    #'   #00FF00).
    bgColor = function(value) { return(invisible(private$p_bgColor)) },

    #' @field textColor The color of the text (as a hex value).
    textColor = function(value) { return(invisible(private$p_textColor)) },

    #' @field hAlign The horizontal alignment of the text:  left, center or right.
    hAlign = function(value) { return(invisible(private$p_hAlign)) },

    #' @field vAlign The vertical alignment of the text:  top, middle or bottom.
    vAlign = function(value) { return(invisible(private$p_vAlign)) },

    #' @field textRotation The rotation angle of the text or 255 for vertical.
    textRotation = function(value) { return(invisible(private$p_textRotation)) },

    #' @field paddingAll The padding to apply to all sides of each cell.
    paddingAll = function(value) { return(invisible(private$p_paddingAll)) },

    #' @field paddingLeft The padding to apply to the left side of each cell.
    paddingLeft = function(value) { return(invisible(private$p_paddingLeft)) },

    #' @field paddingRight The padding to apply to the right side of each cell.
    paddingRight = function(value) { return(invisible(private$p_paddingRight)) },

    #' @field paddingTop The padding to apply to the top of each cell.
    paddingTop = function(value) { return(invisible(private$p_paddingTop)) },

    #' @field paddingBottom The padding to apply to the bottom of each cell.
    paddingBottom = function(value) { return(invisible(private$p_paddingBottom)) },

    #' @field borderAll A list (with elements style, color and width) specifying the border
    #'   settings for all four sides of each cell at once.
    borderAll = function(value) { return(invisible(private$p_borderAll)) },

    #' @field borderLeft A list (with elements style, color and width) specifying the
    #'   border settings for the left border of each cell.
    borderLeft = function(value) { return(invisible(private$p_borderLeft)) },

    #' @field borderRight A list (with elements style, color and width) specifying the
    #'   border settings for the right border of each cell.
    borderRight = function(value) { return(invisible(private$p_borderRight)) },

    #' @field borderTop A list (with elements style, color and width) specifying the border
    #'   settings for the top border of each cell.
    borderTop = function(value) { return(invisible(private$p_borderTop)) },

    #' @field borderBottom A list (with elements style, color and width) specifying the
    #'   border settings for the bottom border of each cell.
    borderBottom = function(value) { return(invisible(private$p_borderBottom)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_baseStyleName = NULL,
    p_isBaseStyle = NULL, # TRUE if this style is equivalent to the base style of the same name
    p_fontName = NULL,
    p_fontSize = NULL,
    p_bold = NULL,
    p_italic = NULL,
    p_bgColor = NULL,
    p_textColor = NULL,
    p_hAlign = NULL,
    p_vAlign = NULL,
    p_textRotation = NULL,
    p_paddingAll = NULL,
    p_paddingLeft = NULL,
    p_paddingRight = NULL,
    p_paddingTop = NULL,
    p_paddingBottom = NULL,
    p_borderAll = NULL,
    p_borderLeft = NULL,
    p_borderRight = NULL,
    p_borderTop = NULL,
    p_borderBottom = NULL,
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
             private$isMatch(border1[["color"]], border2[["color"]]) &&
             private$isMatch(border1[["width"]], border2[["width"]]))
    }
  )
)




