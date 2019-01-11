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


context("QUICK TABLE TESTS")


test_that("quick table", {

  tbl <- qtbl(head(bhmsummary))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Status                  TOC      TrainCategory  PowerType  SchedSpeedMPH  GbttWeekDate   GbttMonth  Origin  Destination  TrainCount  OnTimeArrivals  OnTimeDepartures  TotalArrivalDelayMinutes  TotalDepartureDelayMinutes  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-11-27  2016-12-01     CRE          BHI           2               0                 0                         8                           3  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-04  2016-12-01     CRE          BHI           5               0                 2                        50                          37  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-11  2016-12-01     CRE          BHI           4               0                 0                        27                          15  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-18  2016-12-01     CRE          BHI           5               1                 1                        12                           7  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     CRE          BHI           4               0                 2                        33                          19  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     HHD          BHI           1               0                 0                        11                           9  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Status</td>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">TrainCategory</td>\n    <td class=\"ColumnHeader\">PowerType</td>\n    <td class=\"ColumnHeader\">SchedSpeedMPH</td>\n    <td class=\"ColumnHeader\">GbttWeekDate</td>\n    <td class=\"ColumnHeader\">GbttMonth</td>\n    <td class=\"ColumnHeader\">Origin</td>\n    <td class=\"ColumnHeader\">Destination</td>\n    <td class=\"ColumnHeader\">TrainCount</td>\n    <td class=\"ColumnHeader\">OnTimeArrivals</td>\n    <td class=\"ColumnHeader\">OnTimeDepartures</td>\n    <td class=\"ColumnHeader\">TotalArrivalDelayMinutes</td>\n    <td class=\"ColumnHeader\">TotalDepartureDelayMinutes</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-11-27</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-04</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">50</td>\n    <td class=\"Cell\">37</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-11</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">27</td>\n    <td class=\"Cell\">15</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-18</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">33</td>\n    <td class=\"Cell\">19</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">HHD</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
