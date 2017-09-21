#' Get a built-in theme for styling a table.
#'
#' \code{getTheme} returns the specified theme.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name of the theme to retrieve.
#' @return A TableStyles object.
getTheme <- function(parentTable, themeName=NULL) {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getTheme", themeName, missing(themeName), allowMissing=FALSE, allowNull=FALSE, allowedClasses="character")
  }
  if(themeName=="default") return(getDefaultTheme(parentTable=parentTable))
  else if(themeName=="largeplain") return(getLargePlainTheme(parentTable=parentTable))
  else if(themeName=="compact") return(getCompactTheme(parentTable=parentTable))
  else stop(paste0("getTheme(): Theme '", themeName, "' is not a recognised theme."), call.=FALSE)
}

#' Get the default theme for styling a table.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getDefaultTheme <- function(parentTable, themeName="default") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
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
  TableStyles$tableStyle <- "Table"
  TableStyles$rootStyle <- "RowHeader"
  TableStyles$rowHeaderStyle <- "RowHeader"
  TableStyles$colHeaderStyle <- "ColumnHeader"
  TableStyles$cellStyle <- "Cell"
  TableStyles$totalStyle <- "Cell"
  return(invisible(TableStyles))
}

#' Get the large plain theme for styling a table.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getLargePlainTheme <- function(parentTable, themeName="largeplain") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
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
#' @export
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @return A TableStyles object.
getCompactTheme <- function(parentTable, themeName="compact") {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getPlainTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
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
#' Get a simple coloured theme that can be used to style a table into a custom colour scheme.
#'
#' @export
#' @param parentTable Owning table.
#' @param themeName The name to use as the new theme name.
#' @param colors The set of colours to use when generating the theme (see the Styling vignette for details).
#' @param fontName The name of the font to use, or a comma separated list (for font-fall-back).
#' @return A TableStyles object.
getSimpleColoredTheme <- function(parentTable, themeName="coloredTheme", colors, fontName) {
  if(R6::is.R6Class(parentTable)&&(parentTable$classname=="BasicTable")) argumentCheckMode <- parentTable$argumentCheckMode
  else argumentCheckMode <- 4
  if(argumentCheckMode > 0) {
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", parentTable, missing(parentTable), allowMissing=FALSE, allowNull=FALSE, allowedClasses="BasicTable")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", themeName, missing(themeName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", colors, missing(colors), allowMissing=FALSE, allowNull=FALSE, allowedClasses="list", allowedListElementClasses="character")
    checkArgument(argumentCheckMode, TRUE, "", "getSimpleColoredTheme", fontName, missing(fontName), allowMissing=TRUE, allowNull=FALSE, allowedClasses="character")
  }
  TableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
  TableStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse",
      "border"=paste0("2px solid ", colors$borderColor)
    ))
  TableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"=fontName,
      "font-size"="0.75em",
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
      "font-size"="0.75em",
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
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      "border"=paste0("1px solid ", colors$borderColor),
      "vertical-align"="middle",
      "text-align"="right",
      color=colors$cellColor,
      "background-color"=colors$cellBackgroundColor
    ))
  TableStyles$addStyle(styleName="Total", list(
      "font-family"=fontName,
      "font-size"="0.75em",
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
