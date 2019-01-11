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


context("EXPORT TESTS")


test_that("export as matrix (no headings)", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # output as matrix
  m <- tbl$asMatrix()
  # prepStr(paste(as.character(m), sep=" ", collapse=" "))
  mdata <- "Sale ID 5334 5336 5338 Item Apple Orange Banana Quantity 5 8 6 Price 0.34 0.47 1.34"

  expect_identical(paste(as.character(m), sep=" ", collapse=" "), mdata)
})


test_that("export as matrix (with headings)", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # output as matrix
  m <- tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE)
  # prepStr(paste(as.character(m), sep=" ", collapse=" "))
  mdata <- "Apple Orange Banana 5 8 6 0.34 0.47 1.34"

  expect_identical(paste(as.character(m), sep=" ", collapse=" "), mdata)
})


test_that("export as data frame", {

  # data for the table
  saleIds <- c(5334, 5336, 5338)
  items <- c("Apple", "Orange", "Banana")
  quantities <- c(5, 8, 6)
  prices <- c(0.34452354, 0.4732543, 1.3443243)

  # construct the table
  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(data.frame(saleIds, items, quantities, prices),
              firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("Sale ID", "Item", "Quantity", "Price"),
              columnFormats=list(NULL, NULL, NULL, "%.2f"))

  # output as data frame
  df <- tbl$asDataFrame(firstRowAsColumnNames=TRUE, rawValue=TRUE)
  # prepStr(paste(as.character(df), sep=" ", collapse=" "))
  text <- "c(5334, 5336, 5338) c(1, 3, 2) c(5, 8, 6) c(0.34452354, 0.4732543, 1.3443243)"

  expect_identical(paste(as.character(df), sep=" ", collapse=" "), text)
})
