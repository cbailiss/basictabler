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

prepStr <- function(s) {
  t <- escapeString(s)
  u <- eval(parse(text=paste0("\"", t, "\"")))
  if(s!=u) stop("Unable to escape string!")
  t <- paste0("\thtml <- \"", t, "\"")
  utils::writeClipboard(t)
  return(invisible())
}



context("BASIC TESTS")

test_that("trivial 2x2 table", {

  df <- data.frame(a=1:2, b=3:4)

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE))
  # prepStr(as.character(tbl$getHtml()))
  str <- "a  b  \n1  3  \n2  4  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})

test_that("sample data", {

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
  # prepStr(tbl$print(asCharacter=TRUE))
  # prepStr(as.character(tbl$getHtml()))
	str <- "                TOC  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales             1,404               2,348         3,909               35.9                 60.1  \n       CrossCountry             5,799              10,246        22,928               25.3                 44.7  \n     London Midland            13,036              17,184        48,279               27.0                 35.6  \n      Virgin Trains             3,289               3,864         8,594               38.3                 45.0  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">On-Time Arrivals</td>\n    <td class=\"ColumnHeader\">On-Time Departures</td>\n    <td class=\"ColumnHeader\">Total Trains</td>\n    <td class=\"ColumnHeader\">On-Time Arrival %</td>\n    <td class=\"ColumnHeader\">On-Time Departure %</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Arriva Trains Wales</td>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">CrossCountry</td>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">London Midland</td>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Virgin Trains</td>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})




context("QUICK TABLE TESTS")

test_that("", {

  tbl <- qtbl(head(bhmsummary))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE))
  # prepStr(as.character(tbl$getHtml()))
  str <- "Status                  TOC      TrainCategory  PowerType  SchedSpeedMPH  GbttWeekDate   GbttMonth  Origin  Destination  TrainCount  OnTimeArrivals  OnTimeDepartures  TotalArrivalDelayMinutes  TotalDepartureDelayMinutes  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-11-27  2016-12-01     CRE          BHI           2               0                 0                         8                           3  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-04  2016-12-01     CRE          BHI           5               0                 2                        50                          37  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-11  2016-12-01     CRE          BHI           4               0                 0                        27                          15  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-18  2016-12-01     CRE          BHI           5               1                 1                        12                           7  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     CRE          BHI           4               0                 2                        33                          19  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     HHD          BHI           1               0                 0                        11                           9  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Status</td>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">TrainCategory</td>\n    <td class=\"ColumnHeader\">PowerType</td>\n    <td class=\"ColumnHeader\">SchedSpeedMPH</td>\n    <td class=\"ColumnHeader\">GbttWeekDate</td>\n    <td class=\"ColumnHeader\">GbttMonth</td>\n    <td class=\"ColumnHeader\">Origin</td>\n    <td class=\"ColumnHeader\">Destination</td>\n    <td class=\"ColumnHeader\">TrainCount</td>\n    <td class=\"ColumnHeader\">OnTimeArrivals</td>\n    <td class=\"ColumnHeader\">OnTimeDepartures</td>\n    <td class=\"ColumnHeader\">TotalArrivalDelayMinutes</td>\n    <td class=\"ColumnHeader\">TotalDepartureDelayMinutes</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-11-27</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-04</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">50</td>\n    <td class=\"Cell\">37</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-11</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">27</td>\n    <td class=\"Cell\">15</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-18</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">33</td>\n    <td class=\"Cell\">19</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">HHD</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


