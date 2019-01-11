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


context("FINDING CELLS TESTS")


test_that("finding and conditional formatting", {

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

  # formatting values (explained in the introduction vignette)
  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # create the table
  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  # apply the red formatting
  redStyle <- tbl$createInlineStyle(declarations=list("background-color"="#FFC7CE", "color"="#9C0006"))
  cells <- tbl$findCells(columnNumbers=5:6, minValue=0, maxValue=40, includeNull=FALSE, includeNA=FALSE)
  lst <- lapply(cells, function(cell) {cell$style <- redStyle})
  # apply the yellow formatting
  yellowStyle <- tbl$createInlineStyle(declarations=list("background-color"="#FFEB9C", "color"="#9C5700"))
  cells <- tbl$findCells(columnNumbers=5:6, minValue=40, maxValue=60, includeNull=FALSE, includeNA=FALSE)
  lst <- lapply(cells, function(cell) {cell$style <- yellowStyle})
  # apply the green formatting
  greenStyle <- tbl$createInlineStyle(declarations=list("background-color"="#C6EFCE", "color"="#006100"))
  cells <- tbl$findCells(columnNumbers=5:6, minValue=60, maxValue=100, includeNull=FALSE, includeNA=FALSE)
  lst <- lapply(cells, function(cell) {cell$style <- greenStyle})
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales             1,404               2,348         3,909               35.9                 60.1  \n       CrossCountry             5,799              10,246        22,928               25.3                 44.7  \n     London Midland            13,036              17,184        48,279               27.0                 35.6  \n      Virgin Trains             3,289               3,864         8,594               38.3                 45.0  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">On-Time Arrivals</td>\n    <td class=\"ColumnHeader\">On-Time Departures</td>\n    <td class=\"ColumnHeader\">Total Trains</td>\n    <td class=\"ColumnHeader\">On-Time Arrival %</td>\n    <td class=\"ColumnHeader\">On-Time Departure %</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Arriva Trains Wales</td>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">35.9</td>\n    <td class=\"Cell\" style=\"background-color: #C6EFCE; color: #006100; \">60.1</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">CrossCountry</td>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">25.3</td>\n    <td class=\"Cell\" style=\"background-color: #FFEB9C; color: #9C5700; \">44.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">London Midland</td>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">27.0</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">35.6</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Virgin Trains</td>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\" style=\"background-color: #FFC7CE; color: #9C0006; \">38.3</td>\n    <td class=\"Cell\" style=\"background-color: #FFEB9C; color: #9C5700; \">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
