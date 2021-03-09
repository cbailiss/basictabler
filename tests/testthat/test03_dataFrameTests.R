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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
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
  tbl <- BasicTable$new(compatibility=list(headerCellsAsTD=TRUE))
  tbl$addData(df)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "dtlogical  dtInteger         dtNumeric        dtComplex  dtCharacter  dtFactor      dtDate            dtPOSIXct            dtPOSIXlt  \n     TRUE   -1023411       -14234.2324        -3423-54i            a         A  1980-12-01  1980-12-01 00:07:31  1980-12-01 00:07:31  \n    FALSE          0                -1             0+0i        wfdsg         B  1999-01-02  1999-01-02 15:26:35  1999-01-02 15:26:35  \n     TRUE          1                 0           324-2i       fsdgsg         A  2000-01-01           2000-01-01           2000-01-01  \n    FALSE          4            4234.3  -42354.342+645i         tgsg         X  2003-09-08  2003-09-08 09:30:22  2003-09-08 09:30:22  \n     TRUE       4233  423544255435.234        342+5543i      fsdgsrg         B  2017-09-28  2017-09-28 23:59:59  2017-09-28 23:59:59  \n                                                                                                                                      "
  html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">dtlogical</td>\n    <td class=\"ColumnHeader\">dtInteger</td>\n    <td class=\"ColumnHeader\">dtNumeric</td>\n    <td class=\"ColumnHeader\">dtComplex</td>\n    <td class=\"ColumnHeader\">dtCharacter</td>\n    <td class=\"ColumnHeader\">dtFactor</td>\n    <td class=\"ColumnHeader\">dtDate</td>\n    <td class=\"ColumnHeader\">dtPOSIXct</td>\n    <td class=\"ColumnHeader\">dtPOSIXlt</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">-1023411</td>\n    <td class=\"Cell\">-14234.2324</td>\n    <td class=\"Cell\">-3423-54i</td>\n    <td class=\"Cell\">a</td>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">1980-12-01</td>\n    <td class=\"Cell\">1980-12-01 00:07:31</td>\n    <td class=\"Cell\">1980-12-01 00:07:31</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">-1</td>\n    <td class=\"Cell\">0+0i</td>\n    <td class=\"Cell\">wfdsg</td>\n    <td class=\"Cell\">B</td>\n    <td class=\"Cell\">1999-01-02</td>\n    <td class=\"Cell\">1999-01-02 15:26:35</td>\n    <td class=\"Cell\">1999-01-02 15:26:35</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">324-2i</td>\n    <td class=\"Cell\">fsdgsg</td>\n    <td class=\"Cell\">A</td>\n    <td class=\"Cell\">2000-01-01</td>\n    <td class=\"Cell\">2000-01-01</td>\n    <td class=\"Cell\">2000-01-01</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">FALSE</td>\n    <td class=\"Cell\">4</td>\n    <td class=\"Cell\">4234.3</td>\n    <td class=\"Cell\">-42354.342+645i</td>\n    <td class=\"Cell\">tgsg</td>\n    <td class=\"Cell\">X</td>\n    <td class=\"Cell\">2003-09-08</td>\n    <td class=\"Cell\">2003-09-08 09:30:22</td>\n    <td class=\"Cell\">2003-09-08 09:30:22</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">TRUE</td>\n    <td class=\"Cell\">4233</td>\n    <td class=\"Cell\">423544255435.234</td>\n    <td class=\"Cell\">342+5543i</td>\n    <td class=\"Cell\">fsdgsrg</td>\n    <td class=\"Cell\">B</td>\n    <td class=\"Cell\">2017-09-28</td>\n    <td class=\"Cell\">2017-09-28 23:59:59</td>\n    <td class=\"Cell\">2017-09-28 23:59:59</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n    <td class=\"Cell\">NA</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("multiple row headers (method 1)", {

  # aggregate the sample data to make a small data frame
  library(basictabler)
  library(dplyr)
  tocsummary <- bhmsummary %>%
    group_by(TOC, TrainCategory, PowerType) %>%
    summarise(OnTimeArrivals=sum(OnTimeArrivals),
              OnTimeDepartures=sum(OnTimeDepartures),
              TotalTrains=sum(TrainCount)) %>%
    ungroup() %>%
    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
    arrange(TOC)

  # To specify formatting, a list is created which contains one element for each column in
  # the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
  # The values in the first column in the data frame won't be formatted since NULL has been specified.
  # The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
  # The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
  columnFormats=list(NULL, NULL, NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # render the table directly as a html widget
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE, numberOfColumnsAsRowHeaders=3,
              explicitColumnHeaders=c("TOC", "Category", "Power", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats)

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC            Category  Power  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales   Express Passenger    DMU             1,082               1,859         3,079               35.1                 60.4  \nArriva Trains Wales  Ordinary Passenger    DMU               322                 489           830               38.8                 58.9  \n       CrossCountry   Express Passenger    DMU             5,485               9,920        22,133               24.8                 44.8  \n       CrossCountry   Express Passenger    HST               314                 314           732               42.9                 42.9  \n       CrossCountry  Ordinary Passenger    DMU                 0                  12            63                0.0                 19.0  \n     London Midland   Express Passenger    DMU             1,490               2,084         5,638               26.4                 37.0  \n     London Midland   Express Passenger    EMU             1,271               3,040         8,849               14.4                 34.4  \n     London Midland  Ordinary Passenger    DMU             1,351               1,596         5,591               24.2                 28.5  \n     London Midland  Ordinary Passenger    EMU             8,924              10,464        28,201               31.6                 37.1  \n      Virgin Trains   Express Passenger    DMU               866                 903         2,137               40.5                 42.3  \n      Virgin Trains   Express Passenger    EMU             2,423               2,961         6,457               37.5                 45.9  "
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">Category</th>\n    <th class=\"ColumnHeader\">Power</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,082</td>\n    <td class=\"Cell\">1,859</td>\n    <td class=\"Cell\">3,079</td>\n    <td class=\"Cell\">35.1</td>\n    <td class=\"Cell\">60.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">322</td>\n    <td class=\"Cell\">489</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">38.8</td>\n    <td class=\"Cell\">58.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5,485</td>\n    <td class=\"Cell\">9,920</td>\n    <td class=\"Cell\">22,133</td>\n    <td class=\"Cell\">24.8</td>\n    <td class=\"Cell\">44.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">42.9</td>\n    <td class=\"Cell\">42.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">19.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,490</td>\n    <td class=\"Cell\">2,084</td>\n    <td class=\"Cell\">5,638</td>\n    <td class=\"Cell\">26.4</td>\n    <td class=\"Cell\">37.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">1,271</td>\n    <td class=\"Cell\">3,040</td>\n    <td class=\"Cell\">8,849</td>\n    <td class=\"Cell\">14.4</td>\n    <td class=\"Cell\">34.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,351</td>\n    <td class=\"Cell\">1,596</td>\n    <td class=\"Cell\">5,591</td>\n    <td class=\"Cell\">24.2</td>\n    <td class=\"Cell\">28.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8,924</td>\n    <td class=\"Cell\">10,464</td>\n    <td class=\"Cell\">28,201</td>\n    <td class=\"Cell\">31.6</td>\n    <td class=\"Cell\">37.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">866</td>\n    <td class=\"Cell\">903</td>\n    <td class=\"Cell\">2,137</td>\n    <td class=\"Cell\">40.5</td>\n    <td class=\"Cell\">42.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">2,423</td>\n    <td class=\"Cell\">2,961</td>\n    <td class=\"Cell\">6,457</td>\n    <td class=\"Cell\">37.5</td>\n    <td class=\"Cell\">45.9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("multiple row headers (method 2)", {

  # aggregate the sample data to make a small data frame
  library(basictabler)
  library(dplyr)
  tocsummary <- bhmsummary %>%
    group_by(TOC, TrainCategory, PowerType) %>%
    summarise(OnTimeArrivals=sum(OnTimeArrivals),
              OnTimeDepartures=sum(OnTimeDepartures),
              TotalTrains=sum(TrainCount)) %>%
    ungroup() %>%
    mutate(OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100) %>%
    arrange(TOC)

  # To specify formatting, a list is created which contains one element for each column in
  # the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
  # The values in the first column in the data frame won't be formatted since NULL has been specified.
  # The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
  # The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
  columnFormats=list(NULL, NULL, NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # render the table directly as a html widget
  tbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
              explicitColumnHeaders=c("TOC", "Category", "Power", "On-Time Arrivals", "On-Time Departures",
                                      "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
              columnFormats=columnFormats, columnCellTypes=c("rowHeader", "rowHeader", "rowHeader", "cell", "cell", "cell", "cell", "cell"))

  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- "                TOC            Category  Power  On-Time Arrivals  On-Time Departures  Total Trains  On-Time Arrival %  On-Time Departure %  \nArriva Trains Wales   Express Passenger    DMU             1,082               1,859         3,079               35.1                 60.4  \nArriva Trains Wales  Ordinary Passenger    DMU               322                 489           830               38.8                 58.9  \n       CrossCountry   Express Passenger    DMU             5,485               9,920        22,133               24.8                 44.8  \n       CrossCountry   Express Passenger    HST               314                 314           732               42.9                 42.9  \n       CrossCountry  Ordinary Passenger    DMU                 0                  12            63                0.0                 19.0  \n     London Midland   Express Passenger    DMU             1,490               2,084         5,638               26.4                 37.0  \n     London Midland   Express Passenger    EMU             1,271               3,040         8,849               14.4                 34.4  \n     London Midland  Ordinary Passenger    DMU             1,351               1,596         5,591               24.2                 28.5  \n     London Midland  Ordinary Passenger    EMU             8,924              10,464        28,201               31.6                 37.1  \n      Virgin Trains   Express Passenger    DMU               866                 903         2,137               40.5                 42.3  \n      Virgin Trains   Express Passenger    EMU             2,423               2,961         6,457               37.5                 45.9  "
  html <- "<table class=\"Table\">\n  <tr>\n    <th class=\"ColumnHeader\">TOC</th>\n    <th class=\"ColumnHeader\">Category</th>\n    <th class=\"ColumnHeader\">Power</th>\n    <th class=\"ColumnHeader\">On-Time Arrivals</th>\n    <th class=\"ColumnHeader\">On-Time Departures</th>\n    <th class=\"ColumnHeader\">Total Trains</th>\n    <th class=\"ColumnHeader\">On-Time Arrival %</th>\n    <th class=\"ColumnHeader\">On-Time Departure %</th>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,082</td>\n    <td class=\"Cell\">1,859</td>\n    <td class=\"Cell\">3,079</td>\n    <td class=\"Cell\">35.1</td>\n    <td class=\"Cell\">60.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Arriva Trains Wales</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">322</td>\n    <td class=\"Cell\">489</td>\n    <td class=\"Cell\">830</td>\n    <td class=\"Cell\">38.8</td>\n    <td class=\"Cell\">58.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">5,485</td>\n    <td class=\"Cell\">9,920</td>\n    <td class=\"Cell\">22,133</td>\n    <td class=\"Cell\">24.8</td>\n    <td class=\"Cell\">44.8</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">HST</th>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">314</td>\n    <td class=\"Cell\">732</td>\n    <td class=\"Cell\">42.9</td>\n    <td class=\"Cell\">42.9</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">CrossCountry</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">0</td>\n    <td class=\"Cell\">12</td>\n    <td class=\"Cell\">63</td>\n    <td class=\"Cell\">0.0</td>\n    <td class=\"Cell\">19.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,490</td>\n    <td class=\"Cell\">2,084</td>\n    <td class=\"Cell\">5,638</td>\n    <td class=\"Cell\">26.4</td>\n    <td class=\"Cell\">37.0</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">1,271</td>\n    <td class=\"Cell\">3,040</td>\n    <td class=\"Cell\">8,849</td>\n    <td class=\"Cell\">14.4</td>\n    <td class=\"Cell\">34.4</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">1,351</td>\n    <td class=\"Cell\">1,596</td>\n    <td class=\"Cell\">5,591</td>\n    <td class=\"Cell\">24.2</td>\n    <td class=\"Cell\">28.5</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">London Midland</th>\n    <th class=\"RowHeader\">Ordinary Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">8,924</td>\n    <td class=\"Cell\">10,464</td>\n    <td class=\"Cell\">28,201</td>\n    <td class=\"Cell\">31.6</td>\n    <td class=\"Cell\">37.1</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">DMU</th>\n    <td class=\"Cell\">866</td>\n    <td class=\"Cell\">903</td>\n    <td class=\"Cell\">2,137</td>\n    <td class=\"Cell\">40.5</td>\n    <td class=\"Cell\">42.3</td>\n  </tr>\n  <tr>\n    <th class=\"RowHeader\">Virgin Trains</th>\n    <th class=\"RowHeader\">Express Passenger</th>\n    <th class=\"RowHeader\">EMU</th>\n    <td class=\"Cell\">2,423</td>\n    <td class=\"Cell\">2,961</td>\n    <td class=\"Cell\">6,457</td>\n    <td class=\"Cell\">37.5</td>\n    <td class=\"Cell\">45.9</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
