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




context("CONSTRUCTION TESTS")


test_that("empty", {

  library(basictabler)
  tbl <- BasicTable$new()
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- NULL
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("one cell (1, 1)", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, rawValue=10/3, formattedValue=3.33)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "3.33  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\">3.33</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("one cell (5, 7)", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(5, 7, rawValue=10/3, formattedValue=3.33)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                  \n                  \n                  \n                  \n            3.33  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3.33</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("3x4 table", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setCell(2, 1, cellType="rowHeader", rawValue=5334)
  tbl$cells$setCell(2, 2, cellType="cell", rawValue="Apple")
  tbl$cells$setCell(2, 3, cellType="cell", rawValue=5)
  tbl$cells$setCell(2, 4, cellType="cell", rawValue=0.34)
  tbl$cells$setCell(3, 1, cellType="rowHeader", rawValue=5336)
  tbl$cells$setCell(3, 2, cellType="cell", rawValue="Orange")
  tbl$cells$setCell(3, 3, cellType="cell", rawValue=8)
  tbl$cells$setCell(3, 4, cellType="cell", rawValue=0.47)
  tbl$cells$setCell(4, 1, cellType="rowHeader", rawValue=5338)
  tbl$cells$setCell(4, 2, cellType="cell", rawValue="Banana")
  tbl$cells$setCell(4, 3, cellType="cell", rawValue=6)
  tbl$cells$setCell(4, 4, cellType="cell", rawValue=1.34)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("manipulation 1", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellType="rowHeader", rawValues=c(5334, 5336, 5338))
  tbl$cells$setColumn(2, cellType="cell", rawValues=c("Apple", "Orange", "Banana"))
  tbl$cells$setColumn(3, cellType="cell", rawValues=c(5, 8, 6))
  tbl$cells$setColumn(4, cellType="cell", rawValues=c(0.34, 0.47, 1.34))
  tbl$cells$insertColumn(3)
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Variety")
  tbl$cells$setColumn(3, rawValues=c("Gala", "Jaffa", "Yellow"))
  tbl$cells$insertRow(3)
  tbl$cells$setRow(3, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5335, "Pear", "Marit", 2, 0.89))
  tbl$cells$insertRow(5)
  tbl$cells$setRow(5, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5337, "Plum", "Sweet", 5, 1.59))
  tbl$cells$setCell(1, 6, cellType="columnHeader", rawValue="Total")
  qty <- tbl$cells$getColumnValues(4)
  price <- tbl$cells$getColumnValues(5)
  total <- qty * price
  tbl$cells$setColumn(6, cellType="cell", rawValues=total)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Variety  Quantity  Price  Total  \n   5334   Apple     Gala         5   0.34    1.7  \n   5335    Pear    Marit         2   0.89   1.78  \n   5336  Orange    Jaffa         8   0.47   3.76  \n   5337    Plum    Sweet         5   1.59   7.95  \n   5338  Banana   Yellow         6   1.34   8.04  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Variety</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n    <td class=\"ColumnHeader\">Total</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">Gala</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n    <td class=\"Cell\">1.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5335</td>\n    <td class=\"Cell\">Pear</td>\n    <td class=\"Cell\">Marit</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0.89</td>\n    <td class=\"Cell\">1.78</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">Jaffa</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n    <td class=\"Cell\">3.76</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5337</td>\n    <td class=\"Cell\">Plum</td>\n    <td class=\"Cell\">Sweet</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1.59</td>\n    <td class=\"Cell\">7.95</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">Yellow</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n    <td class=\"Cell\">8.04</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("manipulation 2", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellType="rowHeader", rawValues=c(5334, 5336, 5338))
  tbl$cells$setColumn(2, cellType="cell", rawValues=c("Apple", "Orange", "Banana"))
  tbl$cells$setColumn(3, cellType="cell", rawValues=c(5, 8, 6))
  tbl$cells$setColumn(4, cellType="cell", rawValues=c(0.34, 0.47, 1.34))
  tbl$cells$insertColumn(3)
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Variety")
  tbl$cells$setColumn(3, rawValues=c("Gala", "Jaffa", "Yellow"))
  tbl$cells$insertRow(3)
  tbl$cells$setRow(3, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5335, "Pear", "Marit", 2, 0.89))
  tbl$cells$insertRow(5)
  tbl$cells$setRow(5, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5337, "Plum", "Sweet", 5, 1.59))
  tbl$cells$setCell(1, 6, cellType="columnHeader", rawValue="Total")
  qty <- tbl$cells$getColumnValues(4)
  price <- tbl$cells$getColumnValues(5)
  total <- qty * price
  tbl$cells$setColumn(6, cellType="cell", rawValues=total)
  tbl$cells$deleteColumn(5)
  tbl$cells$deleteRow(3)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Variety  Quantity  Total  \n   5334   Apple     Gala         5    1.7  \n   5336  Orange    Jaffa         8   3.76  \n   5337    Plum    Sweet         5   7.95  \n   5338  Banana   Yellow         6   8.04  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Variety</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Total</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">Gala</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">Jaffa</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">3.76</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5337</td>\n    <td class=\"Cell\">Plum</td>\n    <td class=\"Cell\">Sweet</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">7.95</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">Yellow</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">8.04</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("column-by-column", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellTypes="rowHeader", rawValues=saleIds)
  tbl$cells$setColumn(2, cellTypes="cell", rawValues=items)
  tbl$cells$setColumn(3, cellTypes="cell", rawValues=quantities)
  tbl$cells$setColumn(4, cellTypes="cell", rawValues=prices,
                      formats=list("%.2f"))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("row-by-row", {

  # cell types for the cells in each row
  cellTypes <- c("rowHeader", "cell", "cell", "cell")

  # formats for the values in each row
  # (only the value in the fourth column needs formatting)
  formats <- list(NULL, NULL, NULL, "%.2f")

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setRow(2, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5334, "Apple", 5, 0.34452354))
  tbl$cells$setRow(3, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5336, "Orange", 8, 0.4732543))
  tbl$cells$setRow(4, cellTypes=cellTypes, formats=formats,
                   rawValues=list(5338, "Banana", 6, 1.3443243))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})




