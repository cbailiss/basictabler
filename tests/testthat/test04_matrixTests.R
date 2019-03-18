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



context("MATRIX TESTS")


test_that("empty matrix (1 NA value)", {

  m <- matrix()

  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addMatrix(m)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\">NA</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("1 col, 1 row matrix", {

  m <- matrix(5)

  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addMatrix(m)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "5  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\">5</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("2x3 matrix (numeric)", {

  m <- matrix(1:6, 2, 3, dimnames=list(c("r1", "r2"), c("c1", "c2", "c3")))

  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addMatrix(m, rowNamesAsRowHeaders=TRUE, columnNamesAsColumnHeaders=TRUE)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "    c1  c2  c3  \nr1   1   3   5  \nr2   2   4   6  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\"></td>\n    <td class=\"ColumnHeader\">c1</td>\n    <td class=\"ColumnHeader\">c2</td>\n    <td class=\"ColumnHeader\">c3</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">r1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">r2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">6</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("2x3 matrix (character)", {

  m <- matrix(as.character(1:6), 2, 3, dimnames=list(c("r1", "r2"), c("c1", "c2", "c3")))

  library(basictabler)
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addMatrix(m, rowNamesAsRowHeaders=TRUE, columnNamesAsColumnHeaders=TRUE)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "    c1  c2  c3  \nr1   1   3   5  \nr2   2   4   6  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\"></td>\n    <td class=\"ColumnHeader\">c1</td>\n    <td class=\"ColumnHeader\">c2</td>\n    <td class=\"ColumnHeader\">c3</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">r1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">r2</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">6</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
