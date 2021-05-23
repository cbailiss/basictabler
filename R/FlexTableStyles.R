#' R6 class that defines a collection of styles as used by the `flextable`
#' package.
#'
#' @description
#' The `FlexTableStyles` class stores a collection of `FlexTableStyle`
#' style objects.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format \code{\link{R6Class}} object.
#' @examples
#' # This class should not be used by end users.  It is an internal class
#' # created only by the BasicTable class.  It is used when converting to a
#' # flextable.

FlexTableStyles <- R6::R6Class("FlexTableStyles",
  public = list(

    #' @description
    #' Create a new `FlexTableStyles` object.
    #' @param parentTable Owning table.
    #' @return No return value.
    initialize = function(parentTable) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$new", "Creating new Table FlexTbl Styles...")
      private$p_styles <- list()
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$new", "Created new Table FlexTbl Styles.")
    },

    #' @description
    #' Clear the collection removing all styles.
    #' @return No return value.
    clearStyles = function() {
      if(private$p_parentTable$traceEnabled==TRUE) p
      private$p_parentTable$trace("FlexTableStyles$clearStyles", "Clearing styles...")
      private$p_styles <- list()
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$clearStyles", "Cleared styles.")
    },

    #' @description
    #' Find a style in the collection matching the specified base style name.
    #' @param baseStyleName The style name to find.
    #' @return A `TableTableFlexTbl` object that is the style matching the
    #'   specified base style name or `NULL` otherwise.
    findNamedStyle = function(baseStyleName) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "initialize", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$findNamedStyle", "Finding named style...")

      # This function is a slimmer version of the full function below
      matchedStyle <- NULL
      if(length(private$p_styles)>0) {
        for(i in 1:length(private$p_styles))
          if(private$p_styles[[i]]$isBasicStyleNameMatch(baseStyleName)) {
            matchedStyle <- private$p_styles[[i]]
            break
          }
      }

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$findNamedStyle", "Found named style.")
      return(invisible(matchedStyle))
    },

    #' @description
    #' Find a style in the collection matching the specified base style name and
    #' style properties.  If there is no matching style, then optionally add a
    #' new style.
    #' @param action The action to carry out.  Must be one of "find", "add" or
    #'   "findOrAdd" (default).
    #' @param baseStyleName The style name to find/add.
    #' @param isBaseStyle Is the style to be found/added a base style?
    #' @param style A `TableStyle` object specifying style properties to be found/added.
    #' @param mapFromCss `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding flextable styles, `FALSE` to apply only the specified ft
    #'   styles.
    #' @return A `TableTableFlexTbl` object that is the style matching the
    #'   specified base style name or `NULL` otherwise.
    findOrAddStyle = function(action="findOrAdd", baseStyleName=NULL, isBaseStyle=NULL, style=NULL, mapFromCss=TRUE) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "findOrAddStyle", action, missing(action), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character", allowedValues=c("find", "add", "findOrAdd"))
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "findOrAddStyle", baseStyleName, missing(baseStyleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "findOrAddStyle", isBaseStyle, missing(isBaseStyle), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "findOrAddStyle", style, missing(style), allowMissing=TRUE, allowNull=TRUE, allowedClasses="TableStyle")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "findOrAddStyle", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$findOrAddStyle", "Finding and/or adding style...")

      # This function is used in two different ways:
      # 1. When adding base styles (i.e. named styles in the table) to this collection.
      #    In this case, baseStyleName is the name of the style, isBaseStyle=TRUE (so matching is by name only) and style is the TableStyle object for the base style.
      # 2. When finding styles that have been applied to individual cells using the TableStyle object that is attached to each cell.
      #    In this case, baseStyleName may or may not be present, isBaseStyle=FALSE and style and the style object is the TableStyle object from the cell.

      # font name
      fontName <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-font-name"))
      if(mapFromCss&&(!isTextValue(ftStyleValue))) {
        fontFamilyList <- style$getPropertyValue("font-family")
        if(isTextValue(fontFamilyList)) {
          fontFamilyList <- cleanCssValue(parseCssString(fontFamilyList))
          if(length(fontFamilyList)>0) ftStyleValue <- trimws(fontFamilyList[1])
        }
      }
      if(isTextValue(ftStyleValue)) fontName <- ftStyleValue

      # font size
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-font-size"))
      if(isTextValue(ftStyleValue)) ftStyleValue <- suppressWarnings(max(min(as.numeric(ftStyleValue), 72), 4))
      if(is.null(ftStyleValue)||is.na(ftStyleValue)) ftStyleValue <- NULL
      if(mapFromCss&&(!isTextValue(ftStyleValue))) {
        fontSize <- style$getPropertyValue("font-size")
        ftStyleValue <- parseCssSizeToPt(fontSize)
      }
      if(!isNumericValue(ftStyleValue)) fontSize <- 11
      else fontSize <- ftStyleValue

      # bold
      bold <- FALSE
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-bold"))
      if(isTextValue(ftStyleValue)) {
        if(tolower(ftStyleValue)=="bold") bold <- TRUE
        else bold <- FALSE
      }
      else if(mapFromCss) {
        fontWeight <- cleanCssValue(style$getPropertyValue("font-weight"))
        if(isTextValue(fontWeight)) {
          if(tolower(fontWeight) %in% c("bold", "bolder")) bold <- TRUE
          else {
            fontWeight <- suppressWarnings(as.numeric(fontWeight))
            if((isNumericValue(fontWeight))&&(fontWeight>=600)) bold <- TRUE
          }
        }
      }

      # italic
      italic <- FALSE
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-italic"))
      if(isTextValue(ftStyleValue)) {
        if(tolower(ftStyleValue)=="italic") italic <- TRUE
        else italic <- FALSE
      }
      else if(mapFromCss) {
        fontStyle <- cleanCssValue(style$getPropertyValue("font-style"))
        if(isTextValue(fontStyle)&&(tolower(fontStyle) %in% c("italic", "oblique"))) italic <- TRUE
      }

      # bg color
      bgColor <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-bg-color"))
      check <- grep("#[0-9A-F]{6}", ftStyleValue)
      if((length(check)==0)||(check==FALSE)) ftStyleValue <- NULL
      if(mapFromCss && is.null(ftStyleValue)) {
        backgroundColor <- style$getPropertyValue("background-color")
        ftStyleValue <- parseColor(backgroundColor)
      }
      if(isTextValue(ftStyleValue)) bgColor <- ftStyleValue

      # text color
      textColor <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-text-color"))
      check <- grep("#[0-9A-F]{6}", ftStyleValue)
      if((length(check)==0)||(check==FALSE)) ftStyleValue <- NULL
      if(mapFromCss && is.null(ftStyleValue)) {
        color <- style$getPropertyValue("color")
        ftStyleValue <- parseColor(color)
      }
      if(isTextValue(ftStyleValue)) textColor <- ftStyleValue

      # horizontal alignment
      hAlign <- NULL
      ftStyleValue <- tolower(cleanCssValue(style$getPropertyValue("ft-h-align")))
      if(isTextValue(ftStyleValue)) {
        if(ftStyleValue=="left") hAlign <- "left"
        else if(ftStyleValue=="center") hAlign <- "center"
        else if(ftStyleValue=="right") hAlign <- "right"
      }
      else if(mapFromCss) {
        textAlign <- tolower(cleanCssValue(style$getPropertyValue("text-align")))
        if(isTextValue(textAlign)) {
          if(textAlign=="left") hAlign <- "left"
          else if(textAlign=="center") hAlign <- "center"
          else if(textAlign=="right") hAlign <- "right"
        }
      }

      # vertical alignment
      vAlign <- NULL
      ftStyleValue <- tolower(cleanCssValue(style$getPropertyValue("ft-v-align")))
      if(isTextValue(ftStyleValue)) {
        if(ftStyleValue=="top") vAlign <- "top"
        else if(ftStyleValue=="middle") vAlign <- "center"
        else if(ftStyleValue=="bottom") vAlign <- "bottom"
      }
      else if(mapFromCss) {
        verticalAlign <- tolower(cleanCssValue(style$getPropertyValue("vertical-align")))
        if(isTextValue(verticalAlign)) {
          if(verticalAlign=="top") vAlign <- "top"
          else if(verticalAlign=="text-top") vAlign <- "top"
          else if(verticalAlign=="middle") vAlign <- "center"
          else if(verticalAlign=="text-bottom") vAlign <- "bottom"
          else if(verticalAlign=="bottom") vAlign <- "bottom"
        }
      }

      # text rotation
      textRotation <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-text-rotation"))
      if(isTextValue(ftStyleValue)) {
        if(ftStyleValue=="lrtb") ftStyleValue <- 0
        else if(ftStyleValue=="tblr") ftStyleValue <- 90
        else if(ftStyleValue=="btlr") ftStyleValue <- 270
        else ftStyleValue <- suppressWarnings(max(min(as.numeric(ftStyleValue), 359), 0))
      }
      if(isNumericValue(ftStyleValue)) textRotation <- ftStyleValue

      # padding
      paddingAll <- NULL
      paddingLeft <- NULL
      paddingRight <- NULL
      paddingTop <- NULL
      paddingBottom <- NULL

      # padding all
      # padding can be expressed as 1-4 values:
      # 4 values = top right bottom left
      # 3 values = top left&right bottom
      # 2 values = top&bottom left&right
      # 1 values = top&bottom&left&right
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-padding"))
      if(mapFromCss && is.null(ftStyleValue)) {
        ftStyleValue <- style$getPropertyValue("padding")
      }
      if(!is.null(ftStyleValue)) {
        ftStyleValue <- parseCssString(ftStyleValue, separator=" ")
        if(length(ftStyleValue)==4) {
          paddingTop <- parseCssSizeToPx(ftStyleValue[1])
          paddingRight <- parseCssSizeToPx(ftStyleValue[2])
          paddingBottom <- parseCssSizeToPx(ftStyleValue[3])
          paddingLeft <- parseCssSizeToPx(ftStyleValue[4])
        }
        if(length(ftStyleValue)==3) {
          paddingTop <- parseCssSizeToPx(ftStyleValue[1])
          paddingLeft <- parseCssSizeToPx(ftStyleValue[2])
          paddingRight <- parseCssSizeToPx(ftStyleValue[2])
          paddingBottom <- parseCssSizeToPx(ftStyleValue[3])
        }
        if(length(ftStyleValue)==2) {
          paddingTop <- parseCssSizeToPx(ftStyleValue[1])
          paddingBottom <- parseCssSizeToPx(ftStyleValue[1])
          paddingLeft <- parseCssSizeToPx(ftStyleValue[2])
          paddingRight <- parseCssSizeToPx(ftStyleValue[2])
        }
        if(length(ftStyleValue)==1) {
          paddingAll <- parseCssSizeToPx(ftStyleValue[1])
        }
      }

      # padding left
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-padding-left"))
      if(isTextValue(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      if(mapFromCss && is.null(ftStyleValue)) {
        ftStyleValue <- style$getPropertyValue("padding-left")
        if(!is.null(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      }
      if(isNumericValue(ftStyleValue)&&(ftStyleValue>0)) paddingLeft <- ftStyleValue

      # padding right
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-padding-right"))
      if(isTextValue(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      if(mapFromCss && is.null(ftStyleValue)) {
        ftStyleValue <- style$getPropertyValue("padding-right")
        if(!is.null(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      }
      if(isNumericValue(ftStyleValue)&&(ftStyleValue>0)) paddingRight <- ftStyleValue

      # padding top
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-padding-top"))
      if(isTextValue(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      if(mapFromCss && is.null(ftStyleValue)) {
        ftStyleValue <- style$getPropertyValue("padding-top")
        if(!is.null(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      }
      if(isNumericValue(ftStyleValue)&&(ftStyleValue>0)) paddingTop <- ftStyleValue

      # padding bottom
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-padding-bottom"))
      if(isTextValue(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      if(mapFromCss && is.null(ftStyleValue)) {
        ftStyleValue <- style$getPropertyValue("padding-bottom")
        if(!is.null(ftStyleValue)) ftStyleValue <- parseCssSizeToPx(ftStyleValue)
      }
      if(isNumericValue(ftStyleValue)&&(ftStyleValue>0)) paddingBottom <- ftStyleValue

      # padding limits
      if(!is.null(paddingAll)) paddingAll <- suppressWarnings(max(min(as.numeric(paddingAll), 500), 0))
      if(!is.null(paddingLeft)) paddingLeft <- suppressWarnings(max(min(as.numeric(paddingLeft), 500), 0))
      if(!is.null(paddingRight)) paddingRight <- suppressWarnings(max(min(as.numeric(paddingRight), 500), 0))
      if(!is.null(paddingTop)) paddingTop <- suppressWarnings(max(min(as.numeric(paddingTop), 500), 0))
      if(!is.null(paddingBottom)) paddingBottom <- suppressWarnings(max(min(as.numeric(paddingBottom), 500), 0))

      # border all
      borderAll <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-border"))
      if(isTextValue(ftStyleValue)) borderAll <- parseFtBorder(ftStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border")
        if(isTextValue(cssBorder)) cssBorder <- getFtBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderAll <- cssBorder
      }

      # border left
      borderLeft <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-border-left"))
      if(isTextValue(ftStyleValue)) borderLeft <- parseFtBorder(ftStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-left")
        if(isTextValue(cssBorder)) cssBorder <- getFtBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderLeft <- cssBorder
      }

      # border right
      borderRight <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-border-right"))
      if(isTextValue(ftStyleValue)) borderRight <- parseFtBorder(ftStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-right")
        if(isTextValue(cssBorder)) cssBorder <- getFtBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderRight <- cssBorder
      }

      # border top
      borderTop <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-border-top"))
      if(isTextValue(ftStyleValue)) borderTop <- parseFtBorder(ftStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-top")
        if(isTextValue(cssBorder)) cssBorder <- getFtBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderTop <- cssBorder
      }

      # border bottom
      borderBottom <- NULL
      ftStyleValue <- cleanCssValue(style$getPropertyValue("ft-border-bottom"))
      if(isTextValue(ftStyleValue)) borderBottom <- parseFtBorder(ftStyleValue)
      else if(mapFromCss) {
        cssBorder <- style$getPropertyValue("border-bottom")
        if(isTextValue(cssBorder)) cssBorder <- getFtBorderFromCssBorder(cssBorder)
        if(!is.null(cssBorder)) borderBottom <- cssBorder
      }

      # find a matching style?
      matchedStyle <- NULL
      if(action %in% c("find", "findOrAdd")) {
        if(isBaseStyle) {
          if(length(private$p_styles)>0) {
            for(i in 1:length(private$p_styles))
              if(private$p_styles[[i]]$isBasicStyleNameMatch(baseStyleName)) {
                matchedStyle <- private$p_styles[[i]]
                break
              }
          }
        }
        else {
          if(length(private$p_styles)>0) {
            for(i in 1:length(private$p_styles))
              if(private$p_styles[[i]]$isFullStyleDetailMatch(baseStyleName=baseStyleName, isBaseStyle=isBaseStyle,
                         fontName=fontName, fontSize=fontSize, bold=bold,
                         italic=italic, bgColor=bgColor, textColor=textColor,
                         hAlign=hAlign, vAlign=vAlign, textRotation=textRotation,
                         paddingAll=paddingAll, paddingLeft=paddingLeft, paddingRight=paddingRight,
                         paddingTop=paddingTop, paddingBottom=paddingBottom,
                         borderAll=borderAll, borderLeft=borderLeft, borderRight=borderRight,
                         borderTop=borderTop, borderBottom=borderBottom)) {
                matchedStyle <- private$p_styles[[i]]
                break
              }
          }
        }
      }

      # create the new style?
      if(is.null(matchedStyle) && (action %in% c("add", "findOrAdd"))) {
        matchedStyle <- FlexTableStyle$new(private$p_parentTable,
                                        baseStyleName=baseStyleName, isBaseStyle=isBaseStyle,
                                        fontName=fontName, fontSize=fontSize, bold=bold,
                                        italic=italic, bgColor=bgColor, textColor=textColor,
                                        hAlign=hAlign, vAlign=vAlign,  textRotation=textRotation,
                                        paddingAll=paddingAll, paddingLeft=paddingLeft, paddingRight=paddingRight,
                                        paddingTop=paddingTop, paddingBottom=paddingBottom,
                                        borderAll=borderAll,
                                        borderLeft=borderLeft, borderRight=borderRight,
                                        borderTop=borderTop, borderBottom=borderBottom)
        private$p_styles[[length(private$p_styles)+1]] <- matchedStyle
      }

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$findOrAddStyle", "Found and/or added style.")
      return(matchedStyle)
    },

    #' @description
    #' Populate the FlexTbl styles
    #'   based on the styles defined in the table.
    #' @param mapFromCss `TRUE` (default) to map the basictabler CSS styles to
    #'   corresponding Excel styles, `FALSE` to apply only the specified ft
    #'   styles.
    #' @return No return value.
    addNamedStyles = function(mapFromCss=TRUE) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "addNamedStyles", mapFromCss, missing(mapFromCss), allowMissing=TRUE, allowNull=FALSE, allowedClasses="logical")
      }

      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$addNamedStyles", "Adding named styles...")
      for(i in 1:length(private$p_parentTable$styles$styles)) {
        style <- private$p_parentTable$styles$styles[[i]]
        if(!is.null(self$findNamedStyle(baseStyleName=style$name))) next
        FlexTblStyle <- self$findOrAddStyle(action="add", baseStyleName=style$name, isBaseStyle=TRUE, style=style, mapFromCss=TRUE)
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("FlexTableStyles$addNamedStyles", "Added named styles.")
    },

    #' @description
    #' Return the contents of this object as a list for debugging.
    #' @return A list of various object properties.
    asList = function() {
      lst <- list()
      if(length(private$p_styles) > 0) {
        for (i in 1:length(private$p_styles)) {
          lst[[i]] = private$p_styles[[i]]$asList()
        }
      }
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
    #' @param seperator Delimiter used to combine multiple values into a string.
    #' @return A character representation of various object properties.
    asString = function(seperator=", ") {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "FlexTableStyles", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
       }
      cstr <- ""
       if(length(private$p_styles)>0) {
         for(i in 1:length(private$p_styles)) {
           cg <- private$p_styles[[i]]
           sep <- ""
           if(i > 1) { sep <- seperator }
           cstr <- paste0(cstr, sep, cg$asString())
         }
       }
       return(cstr)
    }
  ),
  active = list(

    #' @field count The number of styles in the collection.
    count = function(value) { return(invisible(length(private$p_styles))) },

    #' @field styles A list of `FlexTableStyle` objects that comprise the collection.
    styles = function(value) { return(invisible(private$p_styles)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_styles = NULL
  )
)
