#' R6 class that specifies styling.
#'
#' @description
#' The `TableStyle` class specifies the styling for headers and cells in a
#' table.  Styles are specified in the form of Cascading Style Sheet (CSS)
#' name-value pairs.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link[R6]{R6Class}} object.
#' @examples
#' # TableStyle objects are normally created indirectly via one of the helper
#' # methods.
#' # For an example, see the `TableStyles` class.

TableStyle <- R6::R6Class("TableStyle",
  public = list(

   #' @description
   #' Create a new `TableStyle` object.
   #' @param parentTable Owning table.
   #' @param styleName A unique name for the style.
   #' @param declarations A list containing CSS style declarations.
   #' Example: `declarations = list(font="...", color="...")`
   #' @return No return value.
   initialize = function(parentTable, styleName=NULL, declarations= NULL) { # declarations = list(font="...", color="...")
     if(parentTable$argumentCheckMode > 0) {
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyle", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyle", "initialize", styleName, missing(styleName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
       checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyle", "initialize", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     private$p_parentTable <- parentTable
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$new", "Creating new Table Style...", list())
     private$p_declarations <- list()
     private$p_name <- styleName
     if(!is.null(declarations)) {
       if(length(declarations)>0) {
         nms <- names(declarations)
         for(i in 1:length(declarations)) {
            nme <- nms[i]
            val <- declarations[[i]]
            if(is.null(nme)) next
            if(is.null(val)) next
            private$p_declarations[[tolower(trimws(nme))]] <- as.character(val)
         }
       }
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$new", "Created new Table Style")
   },

   #' @description
   #' Set the value of a single style property.
   #' @param property The CSS style property name, e.g. color.
   #' @param value The value of the style property, e.g. red.
   #' @return No return value.
   setPropertyValue = function(property=NULL, value=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "setPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "setPropertyValue", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses=c("character", "integer", "numeric"))
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$setPropertyValue", "Setting property value...", list(property=property, value=value))
     private$p_declarations[[tolower(trimws(property))]] <- value
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$setPropertyValue", "Set property value.")
     return(invisible())
   },

   #' @description
   #' Set the values of multiple style properties.
   #' @param declarations A list containing CSS style declarations.
   #' Example: `declarations = list(font="...", color="...")`
   #' @return No return value.
   setPropertyValues = function(declarations=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "setPropertyValues", declarations, missing(declarations), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses=c("character", "integer", "numeric"))
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$setPropertyValues", "Setting property values...")
     nms <- names(declarations)
     if(length(nms)==0) return(invisible())
     for(i in 1:length(nms)) {
       property <- nms[i]
       if(is.null(property)) stop("TableStyle$setPropertyValues():  NULL style property encountered.")
       value <- declarations[[i]]
       private$p_declarations[[tolower(trimws(property))]] <- value
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$setPropertyValues", "Set property values.")
     return(invisible())
   },

   #' @description
   #' Get the value of a single style property.
   #' @param property The CSS style property name, e.g. color.
   #' @return No return value.
   getPropertyValue = function(property=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "getPropertyValue", property, missing(property), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$getPropertyValue", "Getting property value...", list(property=property))
     val <- private$p_declarations[[tolower(trimws(property))]]
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$getPropertyValue", "Got property value.")
     return(invisible(val))
   },

   #' @description
   #' Generate a CSS style rule from this table style.
   #' @param selector The CSS selector name.  Default value `NULL`.
   #' @return The CSS style rule, e.g. \{ text-align: center; color: red; \}
   asCSSRule = function(selector=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$asCSSRule", "Getting CSS rule...")
     cssRule <- NULL
     if(!is.null(selector)) cssRule <- paste0(selector, " {")
     if(length(private$p_declarations)>0) {
       nms <- names(private$p_declarations)
       for(i in 1:length(private$p_declarations)) {
         if(length(nms[i])==0) stop("TableStyle$asCSSRule(): Encountered a style declaration without a name.", call. = FALSE)
         if(startsWith(tolower(nms[i]), "xl-")) next # exclude Excel declarations from CSS/HTML output
         cssRule <- paste0(cssRule, nms[i], ": ", private$p_declarations[[i]], "; ")
       }
     }
     if(!is.null(selector)) cssRule <- paste0(cssRule, "}")
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$asCSSRule", "Got CSS rule.")
     return(invisible(cssRule))
   },

   #' @description
   #' Generate a named CSS style from this table style.
   #' @param styleNamePrefix A character variable specifying a prefix for all named
   #' CSS styles, to avoid style name collisions where multiple tables exist.
   #' @return The CSS style rule, e.g. cell \{ text-align: center; color: red; \}
   asNamedCSSStyle = function(styleNamePrefix=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "asNamedCSSStyle", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
     }
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$asCSSRule", "Getting named CSS rule...")
     if(is.null(styleNamePrefix)) { selector <- paste0(".", private$p_name) }
     else { selector <- paste0(".", styleNamePrefix, private$p_name) }
     cssRule <- self$asCSSRule(selector=selector)
     if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyle$asNamedCSSStyle", "Got named CSS rule.")
     return(invisible(cssRule))
   },

   #' @description
   #' Create a copy of this `TableStyle` object.
   #' @param newStyleName The name of the new style.
   #' @return The new `TableStyle` object.
   getCopy = function(newStyleName=NULL) {
     if(private$p_parentTable$argumentCheckMode > 0) {
       checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyle", "getCopy", newStyleName, missing(newStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
     }
     copy <- TableStyle$new(parentTable=private$p_parentTable, styleName=newStyleName, declarations=private$p_declarations)
     return(invisible(copy))
   },

   #' @description
   #' Return the contents of this object as a list for debugging.
   #' @return A list of various object properties.
   asList = function() {
     lst <- list(
       name = private$p_name,
       declarations = private$p_declarations
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
   }
  ),
  active = list(

    #' @field name The unique name of the style (must be unique among the style
    #'   names in the table theme).
    name = function(value) { return(invisible(private$p_name)) },

    #' @field declarations A list containing CSS style declarations.
    #' Example: `declarations = list(font="...", color="...")`
    declarations = function(value) { return(invisible(private$p_declarations)) }
  ),
  private = list(
    p_parentTable = NULL,
    p_name = NULL,
    p_declarations = NULL
  )
)

# Examples:

# Fonts:
# https://www.w3schools.com/css/css_font.asp
# font-family: "Times New Roman", Times, serif;
# font-style: normal / italic / oblique;
# font-size: 100% / 40px / 0.875em; * 16px=1em, 14px/16=0.875em */
# font-weight: normal / bold;
# font-variant: normal / small-caps;

# Text:
# https://www.w3schools.com/css/css_text.asp
# https://www.w3schools.com/cssref/css_colors_legal.asp
# https://www.w3schools.com/colors/colors_names.asp
# https://www.w3schools.com/cssref/pr_pos_vertical-align.asp
# color: red / #ff0000 / rgb(255,0,0) / rgba(255, 0, 0, 0.3) / hsl(120, 100%, 50%) / hsla(120, 100%, 50%, 0.3)
# text-align: left / center / right / justify
# vertical-align: baseline / length / sub / super / top / text-top / middle / bottom / text-bottom / initial / inherit
# text-decoration: none / underline / line-through / overline
# text-transform: uppercase / lowercase / capitalize
# text-indent: 50px
# letter-spacing: 3px
# line-height: 0.8;
# word-spacing: -5px / 10px;
# text-shadow: 3px 2px red;

# Vertical Text: (probably needs some experimenting to get working)
# https://css-tricks.com/rotated-table-column-headers/
# https://davidwalsh.name/css-vertical-text
# https://css-tricks.com/snippets/css/text-rotation/
# transform: translate(25px, 51px);
# transform: rotate(315deg); rotate(90deg);
# transform-origin: left top 0;
# float: left;  # emulates auto-width

# Background:
# https://www.w3schools.com/css/css_background.asp
# background-color: lightblue;

# Cell spacing, cell padding:
# http://stackoverflow.com/questions/339923/set-cellpadding-and-cellspacing-in-css
# https://www.w3schools.com/css/css_table.asp
# Cell padding:  td {padding: 6px;}
# Cell spacing:  table {border-spacing: 2px;}
# Cell borders:  table th td {border: 1px solid black;}
# Min width:  td {min-width: 100px;}
# Width:  td {width: 100px / 10% / ...}
# Max width:  td {max-width: 100px;}
# Height:  Same three properties: min-height, height, max-height
# Horizontal dividers:  border-bottom: 1px solid #ddd;
# Hoverable table: tr:hover {background-color: #f5f5f5}
# Striped table: tr:nth-child(even) {background-color: #f2f2f2}
