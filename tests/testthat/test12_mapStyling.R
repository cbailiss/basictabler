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



# source data for tests

# aggregate the sample data to make a small data frame
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



context("MAP STYLING TESTS")


test_that("map styling:  smoke tests", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="range",
                mappings=list(0, "green", 10000, "yellow", 35000, "red"))
  tbl$mapStyling(cells=cells, styleProperty="color", valueType="color", mapType="range",
                mappings=list(from=c(0, 10000, 35000), to=c("white", "black", "white")))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">3,909</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">35.9</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">10,246</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">22,928</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">25.3</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">13,036</td>\n    <td class=\"Cell\" style=\"background-color: yellow; color: black; \">17,184</td>\n    <td class=\"Cell\" style=\"background-color: red; color: white; \">48,279</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">27.0</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">8,594</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">38.3</td>\n    <td class=\"Cell\" style=\"background-color: green; color: white; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=text, mapType=value", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="text", mapType="value",
                mappings=list(5799, "bold", 3864, "bold", 3909, "bold", 3289, 100, 48279, 900))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">3,289</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=text, mapType=logic", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="text", mapType="logic",
                mappings=list("v==1404", "bold", "v<3000", 100, "3000<=v<25000", "bold", "v>25000", 900))
  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="text", mapType="logic",
                mappings=list("v==2348", "red"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">1,404</td>\n    <td class=\"Cell\" style=\"font-weight: 100; background-color: red; \">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3,909</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.9</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">5,799</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">10,246</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">22,928</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">25.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">13,036</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">48,279</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">27.0</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3,289</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">3,864</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">8,594</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">38.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=text, mapType=range", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="text", mapType="range",
                mappings=list(0, 100, 2000, "normal", 10000, "bold", 40000, 900))

  # $renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">1,404</td>\n    <td class=\"Cell\" style=\"font-weight: normal; \">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: normal; \">3,909</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.9</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: normal; \">5,799</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">10,246</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">22,928</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">25.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: bold; \">13,036</td>\n    <td class=\"Cell\" style=\"font-weight: bold; \">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">48,279</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">27.0</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: normal; \">3,289</td>\n    <td class=\"Cell\" style=\"font-weight: normal; \">3,864</td>\n    <td class=\"Cell\" style=\"font-weight: normal; \">8,594</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">38.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=number, mapType=value", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="number", mapType="value",
                mappings=list(2348, 700, 3289, 700, 3909, 700, 1401, 100, 48279, 900))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=number, mapType=logic", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="number", mapType="logic",
                mappings=list("v==14184", 700, "v<3000", 100, "3000<=v<20000", 700, "v>20000", 900))
  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="text", mapType="logic",
                mappings=list("v==8594", "red"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">1,404</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,909</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.9</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">5,799</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">10,246</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">22,928</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">25.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">13,036</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 900; \">48,279</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">27.0</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,289</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,864</td>\n    <td class=\"Cell\" style=\"font-weight: 700; background-color: red; \">8,594</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">38.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=number, mapType=range", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="font-weight", valueType="number", mapType="range",
                mappings=list(0, 100, 3000, 700, 15000, 800, 50000, 900))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"font-weight: 100; \">1,404</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">2,348</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,909</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.9</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">5,799</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">10,246</td>\n    <td class=\"Cell\" style=\"font-weight: 800; \">22,928</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">25.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">13,036</td>\n    <td class=\"Cell\" style=\"font-weight: 800; \">17,184</td>\n    <td class=\"Cell\" style=\"font-weight: 800; \">48,279</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">27.0</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,289</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">3,864</td>\n    <td class=\"Cell\" style=\"font-weight: 700; \">8,594</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">38.3</td>\n    <td class=\"Cell\" style=\"font-weight: 100; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=number, mapType=continuous", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$setStyling(cells=cells, declarations=list("color"="white", "background-color"="green"))
  tbl$mapStyling(cells=cells, styleProperty="opacity", valueType="number", mapType="continuous",
                mappings=list(0, 0.35, 50000, 1))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.368; \">1,404</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.381; \">2,348</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.401; \">3,909</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">35.9</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.351; \">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.425; \">5,799</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.483; \">10,246</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.648; \">22,928</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">25.3</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.351; \">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.519; \">13,036</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.573; \">17,184</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.978; \">48,279</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">27.0</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.393; \">3,289</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.4; \">3,864</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.462; \">8,594</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.35; \">38.3</td>\n    <td class=\"Cell\" style=\"color: white; background-color: green; opacity: 0.351; \">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=color, mapType=value", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:6, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="value",
                mappings=list(1404, "red", 5799, "#00ff00", 3909, "rgb(64,64,255)", 3864, "rgba(128,128,255,0.5)", 48279, "green"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\" style=\"background-color: rgb(64,64,255); \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #00ff00; \">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\" style=\"background-color: green; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\" style=\"background-color: rgba(128,128,255,0.5); \">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=color, mapType=logic", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="logic",
                mappings=list("v==2348", "pink", "v<3000", "red", "3000<=v<15000", "yellow", "v>15000", "green"))
  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="text", mapType="logic",
                mappings=list("v==1404", "red"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: pink; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">10,246</td>\n    <td class=\"Cell\" style=\"background-color: green; \">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">13,036</td>\n    <td class=\"Cell\" style=\"background-color: green; \">17,184</td>\n    <td class=\"Cell\" style=\"background-color: green; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=color, mapType=range", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="range",
                mappings=list(0, "red", 3000, "orange", 5000, "yellow", 15000, "green"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: red; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">10,246</td>\n    <td class=\"Cell\" style=\"background-color: green; \">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: yellow; \">13,036</td>\n    <td class=\"Cell\" style=\"background-color: green; \">17,184</td>\n    <td class=\"Cell\" style=\"background-color: green; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: yellow; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  valueType=color, mapType=continuous", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="continuous",
                 mappings=list(0, "red", 3000, "orange", 5000, "yellow", 15000, "green"))

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #ff4d00; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: #ff8100; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: #ffce00; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #ebf500; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: #79bc00; \">10,246</td>\n    <td class=\"Cell\" style=\"background-color: #008000; \">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #329900; \">13,036</td>\n    <td class=\"Cell\" style=\"background-color: #008000; \">17,184</td>\n    <td class=\"Cell\" style=\"background-color: #008000; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: #ffb200; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: #ffcc00; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: #a3d100; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)

  # test color values
  fc <- function(cell) {
    if(is.null(cell$style)) return("-")
    value <- cell$style$getPropertyValue("background-color")
    if(is.null(value)) return("-")
    if(is.na(value)) return("na")
    value <- gsub("#", "", value)
    return(value)
  }
  totalColour <- paste(sapply(cells, fc), collapse="|")
  expect_identical(totalColour, "ff4d00|ff8100|ffce00|ebf500|79bc00|008000|329900|008000|008000|ffb200|ffcc00|a3d100")

  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  mapping function", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  redclr <- function(x, cell) {
    clr <- 255-floor(140*cell$columnNumber/3)
    return(paste0("#",
                  format(as.hexmode(255), width=2),
                  format(as.hexmode(clr), width=2),
                  format(as.hexmode(clr), width=2)))
  }
  tbl$mapStyling(cells=cells, styleProperty="background-color", mappings=redclr)

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: #ff7373; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: #ff4545; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: #ff7373; \">10,246</td>\n    <td class=\"Cell\" style=\"background-color: #ff4545; \">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">13,036</td>\n    <td class=\"Cell\" style=\"background-color: #ff7373; \">17,184</td>\n    <td class=\"Cell\" style=\"background-color: #ff4545; \">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: #ffa2a2; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: #ff7373; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: #ff4545; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  mapping range (3 \"from\", 2 \"to\")", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  # note below there are 3 "from" values and 2 "to" values, and styleHigherValues has been disabled
  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="range",
                mappings=list(0, "red", 3000, "orange", 15000), styleHigherValues=FALSE)

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: red; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("map styling:  mapping range (extended lower)", {

  tbl <- BasicTable$new()
  tbl$addData(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  cells <- tbl$getCells(rowNumbers=2:5, columnNumbers=2:4, matchMode="combinations")

  # note below there are 3 "from" values and 2 "to" values, and styleHigherValues has been disabled
  tbl$mapStyling(cells=cells, styleProperty="background-color", valueType="color", mapType="range",
                mappings=list(2000, "red", 4000, "orange", 15000), styleLowerValues=TRUE, styleHigherValues=FALSE)

  # tbl$renderTable()
  # sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE)
  # prepStr(as.character(tbl$getHtml()))
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <td class=\"Cell\" style=\"background-color: red; \">1,404</td>\n    <td class=\"Cell\" style=\"background-color: red; \">2,348</td>\n    <td class=\"Cell\" style=\"background-color: red; \">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">5,799</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <td class=\"Cell\" style=\"background-color: orange; \">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <td class=\"Cell\" style=\"background-color: red; \">3,289</td>\n    <td class=\"Cell\" style=\"background-color: red; \">3,864</td>\n    <td class=\"Cell\" style=\"background-color: orange; \">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_equal(sum(tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE), na.rm=TRUE), 141191.790555)
  expect_identical(as.character(tbl$getHtml()), html)
})



