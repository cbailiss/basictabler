library(testthat)

# most common expectations:
# equality:        expect_equal() and expect_identical()
# regexp:          expect_match()
# catch-all:       expect_true() and expect_false()
# console output:  expect_output()
# messages:        expect_message()
# warning:         expect_warning()
# errors:          expect_error()

escapeString <- function(s) {
  t <- gsub("(\\\\)", "\\\\\\\\", s)
  t <- gsub("(\n)", "\\\\n", t)
  t <- gsub("(\r)", "\\\\r", t)
  t <- gsub("(\")", "\\\\\"", t)
  return(t)
}

prepStr <- function(s, varName="html") {
  t <- escapeString(s)
  u <- eval(parse(text=paste0("\"", t, "\"")))
  if(s!=u) stop("Unable to escape string!")
  if(is.null(varName)) varName <- "html"
  t <- paste0("\t", varName, " <- \"", t, "\"")
  utils::writeClipboard(t)
  return(invisible())
}



context("THEMING TESTS")



test_that("alt built in theme", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  tbl$theme <- "largeplain"

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.875em; padding: 4px; min-width: 100px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; }\r\n.RowHeader {font-family: Arial; font-size: 0.875em; padding: 4px; min-width: 100px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; }\r\n.Cell {font-family: Arial; font-size: 0.875em; padding: 4px; min-width: 100px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("simple theme 1", {

  # define the colours
  blue1Theme <- list(
    fontName="Verdana, Arial",
    fontSize="0.75em",
    headerBackgroundColor = "rgb(68, 114, 196)",
    headerColor = "rgb(255, 255, 255)",
    cellBackgroundColor = "rgb(255, 255, 255)",
    cellColor = "rgb(0, 0, 0)",
    totalBackgroundColor = "rgb(186, 202, 233)",
    totalColor = "rgb(0, 0, 0)",
    borderColor = "rgb(48, 84, 150)"
  )

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  tbl$theme <- blue1Theme

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(48, 84, 150); }\r\n.ColumnHeader {font-family: Verdana, Arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(48, 84, 150); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(68, 114, 196); }\r\n.RowHeader {font-family: Verdana, Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(48, 84, 150); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(68, 114, 196); }\r\n.Cell {font-family: Verdana, Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(48, 84, 150); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font-family: Verdana, Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(48, 84, 150); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(186, 202, 233); }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})




test_that("simple theme 2", {

  # define the colours
  yellowColors <- list(
    headerBackgroundColor = "rgb(255, 192, 0)",
    headerColor = "rgb(255, 255, 255)",
    cellBackgroundColor="rgb(255, 255, 255)",
    cellColor="rgb(0, 0, 0)",
    totalBackgroundColor = "rgb(255, 242, 204)",
    totalColor="rgb(0, 0, 0)",
    borderColor = "rgb(255, 192, 0)"
  )

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  theme <- getSimpleColoredTblTheme(parentTable=tbl, colors=yellowColors, fontName="Verdana")
  tbl$theme <- theme

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(255, 192, 0); }\r\n.ColumnHeader {font-family: Verdana; font-size: 0.75em; padding: 2px; border: 1px solid rgb(255, 192, 0); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(255, 192, 0); }\r\n.RowHeader {font-family: Verdana; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(255, 192, 0); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(255, 192, 0); }\r\n.Cell {font-family: Verdana; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(255, 192, 0); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font-family: Verdana; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(255, 192, 0); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 242, 204); }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})




test_that("custom theme", {

  createCustomTheme <- function(parentTable=NULL, themeName="myCustomTheme") {
    tableStyles <- TableStyles$new(parentTable=parentTable, themeName=themeName)
    # borders in purple
    tableStyles$addStyle(styleName="Table", list(
      "border-collapse"="collapse",
      "border"="2px solid #B28DFF"
    ))
    # column headings in pink
    tableStyles$addStyle(styleName="ColumnHeader", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px",
      "border"="2px solid #B28DFF",
      "vertical-align"="middle",
      "text-align"="center",
      "font-weight"="bold",
      color="#DB49AC",
      "background-color"="#FFCCF9",
      "xl-wrap-text"="wrap"
    ))
    # row headings in blue
    tableStyles$addStyle(styleName="RowHeader", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px 8px 2px 2px",
      "border"="1px solid #B28DFF",
      "vertical-align"="middle",
      "text-align"="left",
      "font-weight"="bold",
      color="#438EC8",
      "background-color"="#ACE7FF",
      "xl-wrap-text"="wrap"
    ))
    # cells in yellow
    tableStyles$addStyle(styleName="Cell", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      padding="2px 2px 2px 8px",
      "border"="1px solid #B28DFF",
      "text-align"="right",
      color="#FF800D",
      "background-color"="#FFFFD1"
    ))
    # totals in orange
    tableStyles$addStyle(styleName="Total", list(
      "font-family"="\"Courier New\", Courier, monospace",
      "font-size"="0.75em",
      "font-weight"="bold",
      padding="2px 2px 2px 8px",
      "border"="1px solid rgb(84, 130, 53)",
      "text-align"="right",
      color="#3BC6B6",
      "background-color"="#BFFCC6"
    ))
    tableStyles$tableStyle <- "Table"
    tableStyles$rootStyle <- "ColumnHeader"
    tableStyles$rowHeaderStyle <- "RowHeader"
    tableStyles$colHeaderStyle <- "ColumnHeader"
    tableStyles$cellStyle <- "Cell"
    tableStyles$totalStyle <- "Total"
    return(invisible(tableStyles))
  }

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  tbl$theme <- createCustomTheme(tbl)

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {border-collapse: collapse; border: 2px solid #B28DFF; }\r\n.ColumnHeader {font-family: \"Courier New\", Courier, monospace; font-size: 0.75em; font-weight: bold; padding: 2px; border: 2px solid #B28DFF; vertical-align: middle; text-align: center; color: #DB49AC; background-color: #FFCCF9; }\r\n.RowHeader {font-family: \"Courier New\", Courier, monospace; font-size: 0.75em; font-weight: bold; padding: 2px 8px 2px 2px; border: 1px solid #B28DFF; vertical-align: middle; text-align: left; color: #438EC8; background-color: #ACE7FF; }\r\n.Cell {font-family: \"Courier New\", Courier, monospace; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid #B28DFF; text-align: right; color: #FF800D; background-color: #FFFFD1; }\r\n.Total {font-family: \"Courier New\", Courier, monospace; font-size: 0.75em; font-weight: bold; padding: 2px 2px 2px 8px; border: 1px solid rgb(84, 130, 53); text-align: right; color: #3BC6B6; background-color: #BFFCC6; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("styling when creating from df", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)
  df <- data.frame(saleIds, items, quantities, prices)
  colNames <- c("Sale ID", "Item", "Quantity", "Price")
  colFormats <- list(NULL, NULL, NULL, "%.2f")

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))

  # define a new style
  tbl$addStyle(styleName="AltCell", list(
    "font-family"="Arial",
    "font-size"="0.8em",
    "font-weight"="bold",
    padding="2px 2px 2px 8px",
    "border-bottom"="1px solid #9C0006",
    "text-align"="right",
    color="#9C0006",
    "background-color"="#FFC7CE"
  ))
  colStyleNames <- c("Cell", "Cell", "AltCell", "Cell")

  # populate the table
  tbl$addData(df, explicitColumnHeaders=colNames, columnFormats=colFormats, baseStyleNames=colStyleNames)

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n.AltCell {font-family: Arial; font-size: 0.8em; font-weight: bold; padding: 2px 2px 2px 8px; border-bottom: 1px solid #9C0006; text-align: right; color: #9C0006; background-color: #FFC7CE; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"AltCell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"AltCell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"AltCell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("styling when creating cell-by-cell", {

  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))

  # specify a new cell style
  tbl$addStyle(styleName="AltCell", list(
    "font-family"="Arial",
    "font-size"="0.8em",
    "font-weight"="bold",
    padding="2px 2px 2px 8px",
    "border"="2px solid #9C0006",
    "text-align"="right",
    color="#9C0006",
    "background-color"="#FFC7CE"
  ))

  # build the table
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(2, 1, cellType="rowHeader", rawValue=5334)
  tbl$cells$setCell(2, 2, cellType="cell", rawValue="Apple")
  tbl$cells$setCell(3, 1, cellType="rowHeader", rawValue=5336)
  tbl$cells$setCell(3, 2, cellType="cell", rawValue="Orange")

  # use the new style for the following cell (used instead of the theme styling)
  tbl$cells$setCell(2, 3, cellType="cell", rawValue=5, baseStyleName="AltCell")

  # specify an additional style declaration for the following cell (used on top of the theme styling)
  tbl$cells$setCell(3, 3, cellType="cell", rawValue=8, styleDeclarations=list("background-color"="#FFFF00"))

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  \n   5334   Apple         5  \n   5336  Orange         8  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n.AltCell {font-family: Arial; font-size: 0.8em; font-weight: bold; padding: 2px 2px 2px 8px; border: 2px solid #9C0006; text-align: right; color: #9C0006; background-color: #FFC7CE; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"AltCell\">5</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\" style=\"background-color: #FFFF00; \">8</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("styling when creating col-by-col", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))

  # define a new style
  tbl$addStyle(styleName="AltColumn", list(
    "font-family"="Arial",
    "font-size"="0.8em",
    "font-weight"="bold",
    padding="2px 2px 2px 8px",
    "border-bottom"="1px solid #9C0006",
    "text-align"="right",
    color="#9C0006",
    "background-color"="#FFC7CE"
  ))
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=saleIds)
  tbl$cells$setColumn(2, cellTypes="cell", rawValues=items)
  tbl$cells$setColumn(3, cellTypes="cell", rawValues=quantities, baseStyleName="AltColumn")
  tbl$cells$setColumn(4, cellTypes="cell", rawValues=prices,
                      formats=list("%.2f"))

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n.AltColumn {font-family: Arial; font-size: 0.8em; font-weight: bold; padding: 2px 2px 2px 8px; border-bottom: 1px solid #9C0006; text-align: right; color: #9C0006; background-color: #FFC7CE; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"AltColumn\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"AltColumn\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"AltColumn\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})





test_that("styling when creating row-by-row", {

  # cell types for the cells in each row
  cellTypes <- c("rowHeader", "cell", "cell", "cell")

  # formats for the values in each row
  # (only the value in the fourth column needs formatting)
  formats <- list(NULL, NULL, NULL, "%.2f")

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))

  # define a new style
  tbl$addStyle(styleName="AltRowLeftAlign", list(
    "font-family"="Arial",
    "font-size"="0.8em",
    "font-weight"="bold",
    padding="2px 2px 2px 2px",
    "border-bottom"="1px solid #9C0006",
    "text-align"="left",
    color="#9C0006",
    "background-color"="#FFC7CE"
  ))
  tbl$addStyle(styleName="AltRowRightAlign", list(
    "font-family"="Arial",
    "font-size"="0.8em",
    "font-weight"="bold",
    padding="2px 2px 2px 8px",
    "border-bottom"="1px solid #9C0006",
    "text-align"="right",
    color="#9C0006",
    "background-color"="#FFC7CE"
  ))
  styleNames <- c("AltRowLeftAlign", "AltRowRightAlign",
                  "AltRowRightAlign", "AltRowRightAlign")
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setRow(2, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5334, "Apple", 5, 0.34452354))
  tbl$cells$setRow(3, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5336, "Orange", 8, 0.4732543), baseStyleNames=styleNames)
  tbl$cells$setRow(4, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5338, "Banana", 6, 1.3443243))

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n.AltRowLeftAlign {font-family: Arial; font-size: 0.8em; font-weight: bold; padding: 2px 2px 2px 2px; border-bottom: 1px solid #9C0006; text-align: left; color: #9C0006; background-color: #FFC7CE; }\r\n.AltRowRightAlign {font-family: Arial; font-size: 0.8em; font-weight: bold; padding: 2px 2px 2px 8px; border-bottom: 1px solid #9C0006; text-align: right; color: #9C0006; background-color: #FFC7CE; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"AltRowLeftAlign\">5336</td>\n    <td class=\"AltRowRightAlign\">Orange</td>\n    <td class=\"AltRowRightAlign\">8</td>\n    <td class=\"AltRowRightAlign\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})




test_that("styling after creating (legacy)", {

  # define the colours
  orangeColors <- list(
    headerBackgroundColor = "rgb(237, 125, 49)",
    headerColor = "rgb(255, 255, 255)",
    cellBackgroundColor = "rgb(255, 255, 255)",
    cellColor = "rgb(0, 0, 0)",
    totalBackgroundColor = "rgb(248, 198, 165)",
    totalColor = "rgb(0, 0, 0)",
    borderColor = "rgb(198, 89, 17)"
  )

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  theme <- getSimpleColoredTblTheme(parentTable=tbl, colors=orangeColors, fontName="Garamond, arial")
  tbl$theme <- theme

  # apply an additional highlight to one cell (3rd row, 2nd column)
  tbl$setStyling(3, 2, declarations=list("background-color"="#FFFF00"))

  # apply an additional highlight to one cell (3rd row, 3rd column)
  cellHighlight <- tbl$createInlineStyle(declarations=list("background-color"="#00FFFF"))
  cell <- tbl$cells$getCell(3, 3)
  cell$style <- cellHighlight

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(198, 89, 17); }\r\n.ColumnHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.RowHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.Cell {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(248, 198, 165); }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\" style=\"background-color: #FFFF00; \">Orange</td>\n    <td class=\"Cell\" style=\"background-color: #00FFFF; \">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("styling after creating (current)", {

  # define the colours
  simpleOrangeTheme <- list(
    fontName="Garamond, arial",
    headerBackgroundColor = "rgb(237, 125, 49)",
    headerColor = "rgb(255, 255, 255)",
    cellBackgroundColor = "rgb(255, 255, 255)",
    cellColor = "rgb(0, 0, 0)",
    totalBackgroundColor = "rgb(248, 198, 165)",
    totalColor = "rgb(0, 0, 0)",
    borderColor = "rgb(198, 89, 17)"
  )

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # theme the table and render
  tbl$theme <- simpleOrangeTheme

  # apply an additional highlight to one cell (3rd row, 2nd column)
  tbl$setStyling(3, 2, declarations=list("background-color"="#FFFF00"))

  # apply an additional highlight to one cell (4rd row, 2nd and 3rd columns)
  tbl$setStyling(rowNumbers=4, columnNumbers=2:3, declarations=list("background-color"="lightgreen"))

  # apply an additional highlight to one cell (3rd row, 3rd column)
  cellHighlight <- tbl$createInlineStyle(declarations=list("background-color"="#00FFFF"))
  cell <- tbl$cells$getCell(3, 3)
  cell$style <- cellHighlight

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()), "css")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  css <- ".Table {display: table; border-collapse: collapse; border: 2px solid rgb(198, 89, 17); }\r\n.ColumnHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: center; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.RowHeader {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: left; font-weight: bold; color: rgb(255, 255, 255); background-color: rgb(237, 125, 49); }\r\n.Cell {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); }\r\n.Total {font-family: Garamond, arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid rgb(198, 89, 17); vertical-align: middle; text-align: right; color: rgb(0, 0, 0); background-color: rgb(248, 198, 165); }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\" style=\"background-color: #FFFF00; \">Orange</td>\n    <td class=\"Cell\" style=\"background-color: #00FFFF; \">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">Banana</td>\n    <td class=\"Cell\" style=\"background-color: lightgreen; \">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("applying styling multiple times to the same cell", {

  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(data.frame(a = c(1)), columnNamesAsColumnHeaders = F)
  tbl$setStyling(1, 1, declarations = list("font-weight" = "bold"))
  tbl$setStyling(1, 1, declarations = list("text-align" = "center"))

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()))
  # prepStr(as.character(tbl$getHtml()))
  str <- "1  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"font-weight: bold; text-align: center; \">1</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("setStyling (cellType)", {

  # aggregate the sample data to make a small data frame
  library(basictabler)
  library(dplyr)
  tocsummary <- bhmsummary %>%
    group_by(TOC, TrainCategory, PowerType) %>%
    summarise(OnTimeArrivals=sum(OnTimeArrivals),
              OnTimeDepartures=sum(OnTimeDepartures),
              TotalTrains=sum(TrainCount)) %>%
    ungroup() %>%
    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
    arrange(TOC)

  # To specify formatting, a list is created which contains one element for each column in
  # the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
  # The values in the first column in the data frame won't be formatted since NULL has been specified.
  # The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
  # The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
  columnFormats=list(NULL, NULL, NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # render the table directly as a html widget
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "Category", "Power", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  tbl$setStyling(columnNumbers=2:3, cellType="rowHeader")

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()), "css")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC            Category  Power  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales   Express Passenger    DMU             1,082               1,859         3,079               35.1                 60.4  \nArriva Trains Wales  Ordinary Passenger    DMU               322                 489           830               38.8                 58.9  \n       CrossCountry   Express Passenger    DMU             5,485               9,920        22,133               24.8                 44.8  \n       CrossCountry   Express Passenger    HST               314                 314           732               42.9                 42.9  \n       CrossCountry  Ordinary Passenger    DMU                 0                  12            63                0.0                 19.0  \n     London Midland   Express Passenger    DMU             1,490               2,084         5,638               26.4                 37.0  \n     London Midland   Express Passenger    EMU             1,271               3,040         8,849               14.4                 34.4  \n     London Midland  Ordinary Passenger    DMU             1,351               1,596         5,591               24.2                 28.5  \n     London Midland  Ordinary Passenger    EMU             8,924              10,464        28,201               31.6                 37.1  \n      Virgin Trains   Express Passenger    DMU               866                 903         2,137               40.5                 42.3  \n      Virgin Trains   Express Passenger    EMU             2,423               2,961         6,457               37.5                 45.9  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"RowHeader\">Category</th>\n    <th class=\"RowHeader\">Power</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,082</td>\n    <td class=\"Cell\">1,859</td>\n    <td class=\"Cell\">3,079</td>\n    <td class=\"Cell\">35.1</td>\n    <td class=\"Cell\">60.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">322</td>\n    <td class=\"Cell\">489</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">38.8</td>\n    <td class=\"Cell\">58.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5,485</td>\n    <td class=\"Cell\">9,920</td>\n    <td class=\"Cell\">22,133</td>\n    <td class=\"Cell\">24.8</td>\n    <td class=\"Cell\">44.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">42.9</td>\n    <td class=\"Cell\">42.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">19.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,490</td>\n    <td class=\"Cell\">2,084</td>\n    <td class=\"Cell\">5,638</td>\n    <td class=\"Cell\">26.4</td>\n    <td class=\"Cell\">37.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">1,271</td>\n    <td class=\"Cell\">3,040</td>\n    <td class=\"Cell\">8,849</td>\n    <td class=\"Cell\">14.4</td>\n    <td class=\"Cell\">34.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,351</td>\n    <td class=\"Cell\">1,596</td>\n    <td class=\"Cell\">5,591</td>\n    <td class=\"Cell\">24.2</td>\n    <td class=\"Cell\">28.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8,924</td>\n    <td class=\"Cell\">10,464</td>\n    <td class=\"Cell\">28,201</td>\n    <td class=\"Cell\">31.6</td>\n    <td class=\"Cell\">37.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">866</td>\n    <td class=\"Cell\">903</td>\n    <td class=\"Cell\">2,137</td>\n    <td class=\"Cell\">40.5</td>\n    <td class=\"Cell\">42.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">2,423</td>\n    <td class=\"Cell\">2,961</td>\n    <td class=\"Cell\">6,457</td>\n    <td class=\"Cell\">37.5</td>\n    <td class=\"Cell\">45.9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("setStyling (visible)", {

  # aggregate the sample data to make a small data frame
  library(basictabler)
  library(dplyr)
  tocsummary <- bhmsummary %>%
    group_by(TOC, TrainCategory, PowerType) %>%
    summarise(OnTimeArrivals=sum(OnTimeArrivals),
              OnTimeDepartures=sum(OnTimeDepartures),
              TotalTrains=sum(TrainCount)) %>%
    ungroup() %>%
    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
    arrange(TOC)

  # To specify formatting, a list is created which contains one element for each column in
  # the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
  # The values in the first column in the data frame won't be formatted since NULL has been specified.
  # The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
  # The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
  columnFormats=list(NULL, NULL, NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # render the table directly as a html widget
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "Category", "Power", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  tbl$setStyling(columnNumbers=2, visible=FALSE)
  tbl$setStyling(columnNumbers=3, visible=TRUE)

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getCss()), "css")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC            Category  Power  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales   Express Passenger    DMU             1,082               1,859         3,079               35.1                 60.4  \nArriva Trains Wales  Ordinary Passenger    DMU               322                 489           830               38.8                 58.9  \n       CrossCountry   Express Passenger    DMU             5,485               9,920        22,133               24.8                 44.8  \n       CrossCountry   Express Passenger    HST               314                 314           732               42.9                 42.9  \n       CrossCountry  Ordinary Passenger    DMU                 0                  12            63                0.0                 19.0  \n     London Midland   Express Passenger    DMU             1,490               2,084         5,638               26.4                 37.0  \n     London Midland   Express Passenger    EMU             1,271               3,040         8,849               14.4                 34.4  \n     London Midland  Ordinary Passenger    DMU             1,351               1,596         5,591               24.2                 28.5  \n     London Midland  Ordinary Passenger    EMU             8,924              10,464        28,201               31.6                 37.1  \n      Virgin Trains   Express Passenger    DMU               866                 903         2,137               40.5                 42.3  \n      Virgin Trains   Express Passenger    EMU             2,423               2,961         6,457               37.5                 45.9  "
  css <- ".Table {display: table; border-collapse: collapse; }\r\n.ColumnHeader {font-family: Arial; font-size: 0.75em; padding: 2px; border: 1px solid lightgray; vertical-align: middle; text-align: center; font-weight: bold; background-color: #F2F2F2; }\r\n.RowHeader {font-family: Arial; font-size: 0.75em; padding: 2px 8px 2px 2px; border: 1px solid lightgray; vertical-align: middle; text-align: left; font-weight: bold; background-color: #F2F2F2; }\r\n.Cell {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; }\r\n.Total {font-family: Arial; font-size: 0.75em; padding: 2px 2px 2px 8px; border: 1px solid lightgray; vertical-align: middle; text-align: right; font-weight: bold; }\r\n"
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\"></th>\n    <th class=\"ColumnHeader\">Power</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">1,082</td>\n    <td class=\"Cell\">1,859</td>\n    <td class=\"Cell\">3,079</td>\n    <td class=\"Cell\">35.1</td>\n    <td class=\"Cell\">60.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">322</td>\n    <td class=\"Cell\">489</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">38.8</td>\n    <td class=\"Cell\">58.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">5,485</td>\n    <td class=\"Cell\">9,920</td>\n    <td class=\"Cell\">22,133</td>\n    <td class=\"Cell\">24.8</td>\n    <td class=\"Cell\">44.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">HST</td>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">42.9</td>\n    <td class=\"Cell\">42.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">19.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">1,490</td>\n    <td class=\"Cell\">2,084</td>\n    <td class=\"Cell\">5,638</td>\n    <td class=\"Cell\">26.4</td>\n    <td class=\"Cell\">37.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">EMU</td>\n    <td class=\"Cell\">1,271</td>\n    <td class=\"Cell\">3,040</td>\n    <td class=\"Cell\">8,849</td>\n    <td class=\"Cell\">14.4</td>\n    <td class=\"Cell\">34.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">1,351</td>\n    <td class=\"Cell\">1,596</td>\n    <td class=\"Cell\">5,591</td>\n    <td class=\"Cell\">24.2</td>\n    <td class=\"Cell\">28.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">EMU</td>\n    <td class=\"Cell\">8,924</td>\n    <td class=\"Cell\">10,464</td>\n    <td class=\"Cell\">28,201</td>\n    <td class=\"Cell\">31.6</td>\n    <td class=\"Cell\">37.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">866</td>\n    <td class=\"Cell\">903</td>\n    <td class=\"Cell\">2,137</td>\n    <td class=\"Cell\">40.5</td>\n    <td class=\"Cell\">42.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">EMU</td>\n    <td class=\"Cell\">2,423</td>\n    <td class=\"Cell\">2,961</td>\n    <td class=\"Cell\">6,457</td>\n    <td class=\"Cell\">37.5</td>\n    <td class=\"Cell\">45.9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getCss()), css)
  expect_identical(as.character(tbl$getHtml()), html)
})

