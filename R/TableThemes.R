#' Get a built-in theme for styling a table.
#'
#' \code{getTblTheme} returns the specified theme.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name of the theme to retrieve.
#' @return A TableStyles object.
#' @examples
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' tbl$theme <- getTblTheme(tbl, "largeplain")
#' tbl$renderTable()
getTblTheme <- function(parentTable, themeName=NULL) {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getTblTheme", themeName, missing(themeName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
  }
  if(themeName=="default") return(getDefaultTblTheme(parentTable=parentTable))
  else if(themeName=="largeplain") return(getLargePlainTblTheme(parentTable=parentTable))
  else if(themeName=="compact") return(getCompactTblTheme(parentTable=parentTable))
  else if(themeName=="blank") return(getBlankTblTheme(parentTable=parentTable))
  else stop(paste0("getTblTheme(): Theme '", themeName, "' is not a recognised theme."), call.=FALSE)
}

#' Get an empty theme for applying no styling to a table.
#'
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getBlankTblTheme <- function(parentTable, themeName="blank") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getBlankTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getBlankTblTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list())
  TableStyles$addStyle(styleName="ColumnHeader", list())
  TableStyles$addStyle(styleName="RowHeader", list())
  TableStyles$addStyle(styleName="Cell", list())
  TableStyles$addStyle(styleName="Total", list())
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "RowHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Total"
  return(invisible(TableStyles))
}

#' Get the default theme for styling a table.
#'
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getDefaultTblTheme <- function(parentTable, themeName="default") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getDefaultTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getDefaultTblTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
      "display"="table",
      "border-collapse"="collapse"
    ))
  TableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "background-color"="#F2F2F2",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="RowHeader", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px 8px 2px 2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      "background-color"="#F2F2F2",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="Cell", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="right"
    ))
  TableStyles$addStyle(styleName="Total", list(
      "font-family"="Arial",
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="right",
      "font-weight"="bold"
    ))
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "RowHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Total"
  return(invisible(TableStyles))
}

#' Get the large plain theme for styling a table.
#'
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getLargePlainTblTheme <- function(parentTable, themeName="largeplain") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getLargePlainTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getLargePlainTblTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
      "display"="table",
      "border-collapse"="collapse"
    ))
  TableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"="Arial",
      "font-size"="0.875em",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="RowHeader", list(
      "font-family"="Arial",
      "font-size"="0.875em",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="Cell", list(
      "font-family"="Arial",
      "font-size"="0.875em",
      padding="4px",
      "min-width"="100px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="right"
    ))
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "RowHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Cell"
  return(invisible(TableStyles))
}

#' Get the compact theme for styling a table.
#'
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getCompactTblTheme <- function(parentTable, themeName="compact") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getCompactTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getCompactTblTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
      "display"="table",
      "border-collapse"="collapse"
    ))
  TableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"="Arial",
      "font-size"="0.625em",
      padding="2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      "background-color"="#F2F2F2",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="RowHeader", list(
      "font-family"="Arial",
      "font-size"="0.625em",
      padding="2px 4px 2px 2px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      "background-color"="#F2F2F2",
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="Cell", list(
      "font-family"="Arial",
      "font-size"="0.625em",
      padding="2px 2px 2px 6px",
      border="1px solid lightgray",
      "vertical-align"="middle",
      "text-align"="right"
    ))
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "RowHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Cell"
  return(invisible(TableStyles))
}

#' Get a simple coloured theme.
#'
#' Get a simple coloured theme that can be used to style a table into a custom
#' colour scheme.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @param colors The set of colours to use when generating the theme (see
#' the Styling vignette for details).  This parameter exists for
#' backward compatibility.
#' @param fontName The name of the font to use, or a comma separated list
#' (for font-fall-back).  This parameter exists for backward compatibility.
#' @param theme A simple theme specified in the form of a list.  See example
#' for supported list elements (all other elements will be ignored).
#' @return A TableStyles object.
#' @examples
#' simpleOrangeTheme <- list(
#'   fontName="Verdana, Arial",
#'   fontSize="0.75em",
#'   headerBackgroundColor = "rgb(237, 125, 49)",
#'   headerColor = "rgb(255, 255, 255)",
#'   cellBackgroundColor = "rgb(255, 255, 255)",
#'   cellColor = "rgb(0, 0, 0)",
#'   totalBackgroundColor = "rgb(248, 198, 165)",
#'   totalColor = "rgb(0, 0, 0)",
#'   borderColor = "rgb(198, 89, 17)"
#' )
#' library(basictabler)
#' tbl <- qtbl(data.frame(a=1:2, b=3:4))
#' # then
#' tbl$theme <- simpleOrangeTheme
#' # or
#' theme <- getSimpleColoredTblTheme(tbl, theme=simpleOrangeTheme)
#' # make further changes to the theme
#' tbl$theme <- simpleOrangeTheme
#' # theme set, now render table
#' tbl$renderTable()
getSimpleColoredTblTheme <- function(parentTable, themeName="coloredTheme", colors=NULL, fontName=NULL, theme=NULL) {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTblTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTblTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTblTheme", colors, missing(colors), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTblTheme", fontName, missing(fontName), allowMissing=TRUE, allowNull=TRUE, allowedClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTblTheme", theme, missing(theme), allowMissing=TRUE, allowNull=TRUE, allowedClasses="list", allowedListElementClasses="character")
  }
  # take values from theme argument, if they are not specified explicitly
  if(is.null(colors)) colors <- theme
  if(is.null(colors)) stop("getSimpleColoredTheme():  colors must be specified.", call. = FALSE)
  if(is.null(fontName)) fontName <- theme$fontName
  if(is.null(fontName)) fontName <- "Arial"
  fontSize <- theme$fontSize
  if(length(fontSize)==0) fontSize <- "0.75em"
  # build the theme
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
      "display"="table",
      "border-collapse"="collapse",
      "border"=paste0("2px solid ", colors$borderColor)
    ))
  TableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"=fontName,
      "font-size"=fontSize,
      padding="2px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      color=colors$headerColor,
      "background-color"=colors$headerBackgroundColor,
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="RowHeader", list(
      "font-family"=fontName,
      "font-size"=fontSize,
      padding="2px 8px 2px 2px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      color=colors$headerColor,
      "background-color"=colors$headerBackgroundColor,
      "xl-wrap-text"="wrap"
    ))
  TableStyles$addStyle(styleName="Cell", list(
      "font-family"=fontName,
      "font-size"=fontSize,
      padding="2px 2px 2px 8px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="right",
      color=colors$cellColor,
      "background-color"=colors$cellBackgroundColor
    ))
  TableStyles$addStyle(styleName="Total", list(
      "font-family"=fontName,
      "font-size"=fontSize,
      padding="2px 2px 2px 8px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="right",
      color=colors$totalColor,
      "background-color"=colors$totalBackgroundColor
    ))
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "ColumnHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Total"
  return(invisible(TableStyles))
}
