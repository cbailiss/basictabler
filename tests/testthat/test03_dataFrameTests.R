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



context("DATA FRAME TESTS")


test_that("empty data frame (0 cols)", {

  df <- data.frame()

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- NULL
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("empty data frame (1 col, 0 rows)", {

  df <- data.frame(a=integer(0))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("empty data frame (2 cols, 0 rows)", {

  df <- data.frame(a=integer(0), b=character(0))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  b  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("1 col, 1 row data frame", {

  df <- data.frame(a=5)

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  \n5  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("2 cols, 1 row data frame", {

  df <- data.frame(a=5, b="hello")

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a      b  \n5  hello  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">hello</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("1 col, 2 rows data frame", {

  df <- data.frame(a=c(5, 7))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  \n5  \n7  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">7</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("2 cols, 2 rows data frame", {

  df <- data.frame(a=c(5, 7), b=c("hello", "world"))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a      b  \n5  hello  \n7  world  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">hello</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">world</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("1 col, 3 rows data frame", {

  df <- data.frame(a=c(5, 7, 9))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  \n5  \n7  \n9  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("2 cols, 3 rows data frame", {

  df <- data.frame(a=c(5, 7, 9), b=c("hello", "world", "today"))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a      b  \n5  hello  \n7  world  \n9  today  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">hello</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">7</td>\n    <td class=\"Cell\">world</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">9</td>\n    <td class=\"Cell\">today</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("data type tests", {

  df <- data.frame(dtlogical=c(TRUE, FALSE, TRUE, FALSE, TRUE, NA),
                   dtInteger=as.integer(c(-1023411, 0, 1, 4, 4233, NA)),
                   dtNumeric=c(-14234.2324, -1, 0, 4234.3, 423544255435.234, NA),
                   dtComplex=c(-3423-54i, 0, 324-2i, -42354.342+645i, 342+5543i, NA),
                   dtCharacter=c("a", "wfdsg", "fsdgsg", "tgsg", "fsdgsrg", NA),
                   dtFactor=as.factor(c("A", "B", "A", "X", "B", NA)),
                   dtDate=as.Date(c("1980-12-01", "1999-01-02", "2000-01-01", "2003-09-08", "2017-09-28", NA)),
                   dtPOSIXct=as.POSIXct(c("1980-12-01 00:07:31", "1999-01-02 15:26:35", "2000-01-01 00:00:00",
                                          "2003-09-08 09:30:22", "2017-09-28 23:59:59", NA), tz = "UTC"),
                   dtPOSIXlt=as.POSIXlt(c("1980-12-01 00:07:31", "1999-01-02 15:26:35", "2000-01-01 00:00:00",
                                          "2003-09-08 09:30:22", "2017-09-28 23:59:59", NA), tz = "UTC"))

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "dtlogical  dtInteger         dtNumeric        dtComplex  dtCharacter  dtFactor      dtDate            dtPOSIXct            dtPOSIXlt  \n     TRUE   -1023411       -14234.2324        -3423-54i            a         A  1980-12-01  1980-12-01 00:07:31  1980-12-01 00:07:31  \n    FALSE          0                -1             0+0i        wfdsg         B  1999-01-02  1999-01-02 15:26:35  1999-01-02 15:26:35  \n     TRUE          1                 0           324-2i       fsdgsg         A  2000-01-01           2000-01-01           2000-01-01  \n    FALSE          4            4234.3  -42354.342+645i         tgsg         X  2003-09-08  2003-09-08 09:30:22  2003-09-08 09:30:22  \n     TRUE       4233  423544255435.234        342+5543i      fsdgsrg         B  2017-09-28  2017-09-28 23:59:59  2017-09-28 23:59:59  \n                                                                                                                                      "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">dtlogical</td>\n    <td class=\"ColumnHeader\">dtInteger</td>\n    <td class=\"ColumnHeader\">dtNumeric</td>\n    <td class=\"ColumnHeader\">dtComplex</td>\n    <td class=\"ColumnHeader\">dtCharacter</td>\n    <td class=\"ColumnHeader\">dtFactor</td>\n    <td class=\"ColumnHeader\">dtDate</td>\n    <td class=\"ColumnHeader\">dtPOSIXct</td>\n    <td class=\"ColumnHeader\">dtPOSIXlt</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">-1023411</td>\n    <td class=\"Cell\">-14234.2324</td>\n    <td class=\"Cell\">-3423-54i</td>\n    <td class=\"Cell\">a</td>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">1980-12-01</td>\n    <td class=\"Cell\">1980-12-01 00:07:31</td>\n    <td class=\"Cell\">1980-12-01 00:07:31</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">-1</td>\n    <td class=\"Cell\">0+0i</td>\n    <td class=\"Cell\">wfdsg</td>\n    <td class=\"Cell\">B</td>\n    <td class=\"Cell\">1999-01-02</td>\n    <td class=\"Cell\">1999-01-02 15:26:35</td>\n    <td class=\"Cell\">1999-01-02 15:26:35</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">324-2i</td>\n    <td class=\"Cell\">fsdgsg</td>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">2000-01-01</td>\n    <td class=\"Cell\">2000-01-01</td>\n    <td class=\"Cell\">2000-01-01</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4234.3</td>\n    <td class=\"Cell\">-42354.342+645i</td>\n    <td class=\"Cell\">tgsg</td>\n    <td class=\"Cell\">X</td>\n    <td class=\"Cell\">2003-09-08</td>\n    <td class=\"Cell\">2003-09-08 09:30:22</td>\n    <td class=\"Cell\">2003-09-08 09:30:22</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4233</td>\n    <td class=\"Cell\">423544255435.234</td>\n    <td class=\"Cell\">342+5543i</td>\n    <td class=\"Cell\">fsdgsrg</td>\n    <td class=\"Cell\">B</td>\n    <td class=\"Cell\">2017-09-28</td>\n    <td class=\"Cell\">2017-09-28 23:59:59</td>\n    <td class=\"Cell\">2017-09-28 23:59:59</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
