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


# sample data

library(basictabler)
library(dplyr)
tocsummary <- bhmsummary %>%
  group_by(TOC) %>%
  summarise(OnTimeArrivals=sum(OnTimeArrivals),
            OnTimeDepartures=sum(OnTimeDepartures),
            TotalTrains=sum(TrainCount)) %>%
  ungroup() %>%
  mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
         OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
  arrange(TOC)


context("BASIC TESTS")


test_that("trivial 2x2 table", {

  df <- data.frame(a=1:2, b=3:4)

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  b  \n1  3  \n2  4  "
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">a</th>\n    <th class=\"ColumnHeader\">b</th>\n  </tr>\n  <tr>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})

test_that("sample data", {

  library(basictabler)
  tbl <- BasicTable$new()
  columnFormats=list()
  columnFormats[[2]] <- list(big.mark=",")
  columnFormats[[3]] <- list(big.mark=",")
  columnFormats[[4]] <- list(big.mark=",")
  columnFormats[[5]] <- "%.1f"
  columnFormats[[6]] <- "%.1f"
  tbl$addData(tocsummary, columnNamesAsColumnHeaders=FALSE, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales             1,404               2,348         3,909               35.9                 60.1  \n       CrossCountry             5,799              10,246        22,928               25.3                 44.7  \n     London Midland            13,036              17,184        48,279               27.0                 35.6  \n      Virgin Trains             3,289               3,864         8,594               38.3                 45.0  "
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
