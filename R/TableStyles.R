#' A class that defines a collection of styles.
#'
#' The TableStyles class defines all of the base styles needed to style/theme a
#' table.  It also defines the names of the styles that are used for
#' styling the different parts of the table.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @return Object of \code{\link{R6Class}} with properties and methods that
#'   define styles/a theme for a table.
#' @format \code{\link{R6Class}} object.
#' @examples
#' # Creating styles is part of defining a theme for a table.
#' # Multiple styles must be created for each theme.
#' # The example below shows how to create one style.
#' # For an example of creating a full theme please
#' # see the Styling vignette.
#' tbl <- BasicTable$new()
#' # ...
#' TableStyles <- TableStyles$new(tbl, themeName="compact")
#' TableStyles$addStyle(styleName="MyNewStyle", list(
#'     font="0.75em arial",
#'     padding="2px",
#'     border="1px solid lightgray",
#'     "vertical-align"="middle",
#'     "text-align"="center",
#'     "font-weight"="bold",
#'     "background-color"="#F2F2F2"
#'   ))
#' @field parentTable Owning table.
#' @field themeName The name of the theme.
#' @field allowExternalStyles Enables integration scenarios where an external
#'   system is supplying the CSS definitions.
#' @field tableStyle The name of the style for the HTML table element.
#' @field rootStyle The name of the style for the HTML cell at the top left of
#'   the table (when both row and column headers are displayed).
#' @field rowHeaderStyle The name of the style for the row headers in the table.
#' @field colHeaderStyle The name of the style for the column headers in the
#'   table.
#' @field cellStyle The name of the cell style for the non-total cells in the
#'   body of the table.
#' @field totalStyle The name of the cell style for the total cells in the
#'   table.

#' @section Methods:
#' \describe{
#'   \item{Documentation}{For more complete explanations and examples please see
#'   the extensive vignettes supplied with this package.}
#'   \item{\code{new(...)}}{Create a new set of styles, specifying the field
#'   values documented above.}
#'
#'   \item{\code{isExistingStyle(styleName)}}{Check whether the specified style
#'   exists.}
#'   \item{\code{getStyle(styleName)}}{Get the specified style.}
#'   \item{\code{addStyle(styleName, declarations)}}{Add a new style to the
#'   collection of styles.}
#'   \item{\code{copyStyle(styleName, newStyleName)}}{Create a copy of a style
#'   with the specified name.}
#'   \item{\code{asCSSRule(styleName, selector)}}{Get a style definition in the
#'   form of a CSS rule.}
#'   \item{\code{asNamedCSSStyle(styleName, styleNamePrefix)}}{Get a style
#'   definition in the form of a named CSS style.}
#'   \item{\code{asList()}}{Get a list representation of the styles.}
#'   \item{\code{asJSON()}}{Get a JSON representation of the styles.}
#'   \item{\code{asString()}}{Get a text representation of the styles.}
#' }

TableStyles <- R6::R6Class("TableStyles",
  public = list(
    initialize = function(parentTable, themeName=NULL, allowExternalStyles=FALSE) {
      if(parentTable$argumentCheckMode > 0) {
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyles", "initialize", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyles", "initialize", themeName, missing(themeName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
        checkArgument(parentTable$argumentCheckMode, FALSE, "TableStyles", "initialize", allowExternalStyles, missing(allowExternalStyles), allowMissing=TRUE, allowNull=TRUE, allowedClasses="logical")
      }
      private$p_parentTable <- parentTable
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$new", "Creating new Table Styles...")
      private$p_theme <- themeName
      private$p_allowExternalStyles <- allowExternalStyles
      private$p_styles <- list()
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$new", "Created new Table Styles.")
    },
    isExistingStyle = function(styleName=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "isExistingStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$isExistingStyle", "Checking style exists...", list(styleName=styleName))
      styleExists <- styleName %in% names(private$p_styles)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$isExistingStyle", "Checked style exists.")
      return(invisible(styleExists))
    },
    getStyle = function(styleName=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "getStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$getStyle", "Getting style...", list(styleName=styleName))
      style <- private$p_styles[[styleName]]
      if(is.null(style)) {
        stop(paste0("TableStyles$getStyle(): No style exists with the name '", styleName, "'"), call. = FALSE)
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$getStyle", "Got style.")
      return(invisible(style))
    },
    addStyle = function(styleName=NULL, declarations= NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "addStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "addStyle", declarations, missing(declarations), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$addStyle", "Adding style...", list(styleName=styleName))
      if(styleName %in% names(private$p_styles)) {
        stop(paste0("TableStyles$addStyle():  A style already exists",
                    " with the name '", styleName, "'.  styleName must unique."), call. = FALSE)
      }
      style <- TableStyle$new(private$p_parentTable, styleName, declarations)
      private$p_styles[[styleName]] <- style
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$addStyle", "Added style.")
      return(invisible(style))
    },
    copyStyle = function(styleName=NULL, newStyleName=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "copyStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "copyStyle", newStyleName, missing(newStyleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$copyStyle", "Copying style...", list(styleName=styleName, newStyleName=newStyleName))
      style <- self$getStyle(styleName=styleName)
      newStyle <- self$addStyle(styleName=newStyleName, declarations=style$declarations)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$copyStyle", "Copied style.")
      return(invisible(newStyle))
   },
    asCSSRule = function(styleName=NULL, selector=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "asCSSRule", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "asCSSRule", selector, missing(selector), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$asCSSRule", "Getting style as CSS rule...", list(styleName=styleName))
      style <- self$getStyle(styleName)
      cssRule <- style$asCSSRule(selector=selector)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$asCSSRule", "Got style as CSS rule.")
      return(invisible(cssRule))
    },
    asNamedCSSStyle = function(styleName=NULL, styleNamePrefix=NULL) {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "asNamedCSSStyle", styleName, missing(styleName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "asNamedCSSStyle", styleNamePrefix, missing(styleNamePrefix), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
      }
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$asNamedCSSStyle", "Getting style as named CSS rule...", list(styleName=styleName, styleNamePrefix=styleNamePrefix))
      style <- self$getStyle(styleName)
      cssRule <- style$asNamedCSSStyle(styleNamePrefix=styleNamePrefix)
      if(private$p_parentTable$traceEnabled==TRUE) private$p_parentTable$trace("TableStyles$asNamedCSSStyle", "Got style as named CSS rule.")
      return(invisible(cssRule))
    },
    asList = function() {
      lst <- list()
      if(length(private$p_styles) > 0) {
        groupNames <- names(private$p_styles)
        for (i in 1:length(private$p_styles)) {
          groupName <- groupNames[i]
          lst[[groupName]] = private$p_styles[[groupName]]$asList()
        }
      }
      return(invisible(lst))
    },
    asJSON = function() { return(jsonlite::toJSON(self$asList())) },
    asString = function(seperator=", ") {
      if(private$p_parentTable$argumentCheckMode > 0) {
        checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "asString", seperator, missing(seperator), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
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
    count = function(value) { return(invisible(length(private$p_styles))) },
    theme = function(value) { return(invisible(private$p_theme)) },
    styles = function(value) { return(invisible(private$p_styles)) },
    allowExternalStyles = function(value) {
      if(missing(value)) return(invisible(private$p_allowExternalStyles))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "allowExternalStyles", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="logical")
        }
        private$p_allowExternalStyles <- value
        return(invisible())
      }
    },
    tableStyle = function(value) {
      if(missing(value)) return(invisible(private$p_tableStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "tableStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$tableStyle: '", value, "' style not found in styles list."))
        }
        private$p_tableStyle <- value
        return(invisible())
      }
    },
    rootStyle = function(value) {
      if(missing(value)) return(invisible(private$p_rootStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "rootStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$rootStyle: '", value, "' style not found in styles list."))
        }
        private$p_rootStyle <- value
        return(invisible())
      }
    },
    rowHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_rowHeaderStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "rowHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$rowHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_rowHeaderStyle <- value
        return(invisible())
      }
    },
    colHeaderStyle = function(value) {
      if(missing(value)) return(invisible(private$p_colHeaderStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "colHeaderStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$colHeaderStyle: '", value, "' style not found in styles list."))
        }
        private$p_colHeaderStyle <- value
        return(invisible())
      }
    },
    cellStyle = function(value) {
      if(missing(value)) return(invisible(private$p_cellStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "cellStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$cellStyle: '", value, "' style not found in styles list."))
        }
        private$p_cellStyle <- value
        return(invisible())
      }
    },
    totalStyle = function(value) {
      if(missing(value)) return(invisible(private$p_totalStyle))
      else {
        if(private$p_parentTable$argumentCheckMode > 0) {
          checkArgument(private$p_parentTable$argumentCheckMode, FALSE, "TableStyles", "totalStyle", value, missing(value), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
        }
        if(private$p_allowExternalStyles==FALSE) {
          if(!(value %in% names(private$p_styles))) stop(paste0("TableStyles$totalStyle: '", value, "' style not found in styles list."))
        }
        private$p_totalStyle <- value
        return(invisible())
      }
    }
  ),
  private = list(
    p_parentTable = NULL,
    p_theme = NULL,
    p_allowExternalStyles = FALSE,
    p_styles = NULL,
    p_tableStyle = NULL,
    p_rootStyle = NULL,
    p_rowHeaderStyle = NULL,
    p_colHeaderStyle = NULL,
    p_cellStyle = NULL,
    p_totalStyle = NULL
  )
)
