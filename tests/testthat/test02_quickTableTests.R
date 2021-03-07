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

  bhmsummary2 <- bhmsummary %>%
    mutate(TotalArrivalDelayMins=as.integer(TotalArrivalDelayMinutes)) %>%
    mutate(TotalDepartureDelayMins=as.integer(TotalDepartureDelayMinutes)) %>%
    select(-TotalArrivalDelayMinutes, -TotalDepartureDelayMinutes)

  tbl <- qtbl(head(bhmsummary2), compatibility=list(headerCellsAsTD=TRUE))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "Status                  TOC      TrainCategory  PowerType  SchedSpeedMPH  GbttWeekDate   GbttMonth  Origin  Destination  TrainCount  OnTimeArrivals  OnTimeDepartures  TotalArrivalDelayMins  TotalDepartureDelayMins  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-11-27  2016-12-01     CRE          BHI           2               0                 0                      8                        3  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-04  2016-12-01     CRE          BHI           5               0                 2                     50                       37  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-11  2016-12-01     CRE          BHI           4               0                 0                     27                       15  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-18  2016-12-01     CRE          BHI           5               1                 1                     12                        7  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     CRE          BHI           4               0                 2                     33                       19  \n     A  Arriva Trains Wales  Express Passenger        DMU             75    2016-12-25  2016-12-01     HHD          BHI           1               0                 0                     11                        9  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">Status</td>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">TrainCategory</td>\n    <td class=\"ColumnHeader\">PowerType</td>\n    <td class=\"ColumnHeader\">SchedSpeedMPH</td>\n    <td class=\"ColumnHeader\">GbttWeekDate</td>\n    <td class=\"ColumnHeader\">GbttMonth</td>\n    <td class=\"ColumnHeader\">Origin</td>\n    <td class=\"ColumnHeader\">Destination</td>\n    <td class=\"ColumnHeader\">TrainCount</td>\n    <td class=\"ColumnHeader\">OnTimeArrivals</td>\n    <td class=\"ColumnHeader\">OnTimeDepartures</td>\n    <td class=\"ColumnHeader\">TotalArrivalDelayMins</td>\n    <td class=\"ColumnHeader\">TotalDepartureDelayMins</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-11-27</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-04</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">50</td>\n    <td class=\"Cell\">37</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-11</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">27</td>\n    <td class=\"Cell\">15</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-18</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">7</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">CRE</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">33</td>\n    <td class=\"Cell\">19</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">Arriva Trains Wales</td>\n    <td class=\"Cell\">Express Passenger</td>\n    <td class=\"Cell\">DMU</td>\n    <td class=\"Cell\">75</td>\n    <td class=\"Cell\">2016-12-25</td>\n    <td class=\"Cell\">2016-12-01</td>\n    <td class=\"Cell\">HHD</td>\n    <td class=\"Cell\">BHI</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">11</td>\n    <td class=\"Cell\">9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})




# sample data for following tests

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



test_that("quick table with simple theming", {

  # column formats
  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # simple theme
  simpleBlueTheme <- list(
    fontName="Verdana, Arial",
    headerBackgroundColor = "rgb(68, 114, 196)",
    headerColor = "rgb(255, 255, 255)",
    cellBackgroundColor = "rgb(255, 255, 255)",
    cellColor = "rgb(0, 0, 0)",
    totalBackgroundColor = "rgb(186, 202, 233)",
    totalColor = "rgb(0, 0, 0)",
    borderColor = "rgb(48, 84, 150)"
  )

  # headings in red text, cells in light gray
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats, theme=simpleBlueTheme,
              compatibility=list(headerCellsAsTD=TRUE))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales             1,404               2,348         3,909               35.9                 60.1  \n       CrossCountry             5,799              10,246        22,928               25.3                 44.7  \n     London Midland            13,036              17,184        48,279               27.0                 35.6  \n      Virgin Trains             3,289               3,864         8,594               38.3                 45.0  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">On-Time Arrivals</td>\n    <td class=\"ColumnHeader\">On-Time Departures</td>\n    <td class=\"ColumnHeader\">Total Trains</td>\n    <td class=\"ColumnHeader\">On-Time Arrival %</td>\n    <td class=\"ColumnHeader\">On-Time Departure %</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Arriva Trains Wales</td>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">CrossCountry</td>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">London Midland</td>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Virgin Trains</td>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})



test_that("quick table with cell type styling", {

  # column formats
  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # create the table
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=FALSE,
              explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats,
              tableStyle=list("border-color"="maroon"),
              headingStyle=list("color"="cornsilk", "background-color"="maroon",
                                "font-style"="italic", "border-color"="maroon"),
              cellStyle=list("color"="maroon", "background-color"="cornsilk",
                             "border-color"="maroon"),
              compatibility=list(headerCellsAsTD=TRUE))

  # set column alignment of first column
  tbl$setStyling(2, 1, 5, 1, declarations=list("text-align"="left"))
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales             1,404               2,348         3,909               35.9                 60.1  \n       CrossCountry             5,799              10,246        22,928               25.3                 44.7  \n     London Midland            13,036              17,184        48,279               27.0                 35.6  \n      Virgin Trains             3,289               3,864         8,594               38.3                 45.0  "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">On-Time Arrivals</td>\n    <td class=\"ColumnHeader\">On-Time Departures</td>\n    <td class=\"ColumnHeader\">Total Trains</td>\n    <td class=\"ColumnHeader\">On-Time Arrival %</td>\n    <td class=\"ColumnHeader\">On-Time Departure %</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\" style=\"text-align: left; \">Arriva Trains Wales</td>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\" style=\"text-align: left; \">CrossCountry</td>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\" style=\"text-align: left; \">London Midland</td>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\" style=\"text-align: left; \">Virgin Trains</td>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
