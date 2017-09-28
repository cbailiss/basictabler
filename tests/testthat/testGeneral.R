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
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">a</td>\n    <td class=\"ColumnHeader\">b</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">1</td>\n    <td class=\"Cell\">3</td>\n  </tr>\n  <tr>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">4</td>\n  </tr>\n</table>"

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
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"ColumnHeader\">TOC</td>\n    <td class=\"ColumnHeader\">On-Time Arrivals</td>\n    <td class=\"ColumnHeader\">On-Time Departures</td>\n    <td class=\"ColumnHeader\">Total Trains</td>\n    <td class=\"ColumnHeader\">On-Time Arrival %</td>\n    <td class=\"ColumnHeader\">On-Time Departure %</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Arriva Trains Wales</td>\n    <td class=\"Cell\">1,404</td>\n    <td class=\"Cell\">2,348</td>\n    <td class=\"Cell\">3,909</td>\n    <td class=\"Cell\">35.9</td>\n    <td class=\"Cell\">60.1</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">CrossCountry</td>\n    <td class=\"Cell\">5,799</td>\n    <td class=\"Cell\">10,246</td>\n    <td class=\"Cell\">22,928</td>\n    <td class=\"Cell\">25.3</td>\n    <td class=\"Cell\">44.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">London Midland</td>\n    <td class=\"Cell\">13,036</td>\n    <td class=\"Cell\">17,184</td>\n    <td class=\"Cell\">48,279</td>\n    <td class=\"Cell\">27.0</td>\n    <td class=\"Cell\">35.6</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">Virgin Trains</td>\n    <td class=\"Cell\">3,289</td>\n    <td class=\"Cell\">3,864</td>\n    <td class=\"Cell\">8,594</td>\n    <td class=\"Cell\">38.3</td>\n    <td class=\"Cell\">45.0</td>\n  </tr>\n</table>"

  expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


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


context("CONSTRUCTION TESTS")


test_that("empty", {

  library(basictabler)
  tbl <- BasicTable$new()
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
  str <- NULL
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\" style=\"text-align: center; padding: 6px\">(no data)</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("one cell (1, 1)", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, rawValue=10/3, formattedValue=3.33)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
	str <- "3.33  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\">3.33</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("one cell (5, 7)", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(5, 7, rawValue=10/3, formattedValue=3.33)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
	str <- "                  \n                  \n                  \n                  \n            3.33  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n  </tr>\n  <tr>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\"></td>\n    <td class=\"Cell\">3.33</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("3x4 table", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setCell(2, 1, cellType="rowHeader", rawValue=5334)
  tbl$cells$setCell(2, 2, cellType="cell", rawValue="Apple")
  tbl$cells$setCell(2, 3, cellType="cell", rawValue=5)
  tbl$cells$setCell(2, 4, cellType="cell", rawValue=0.34)
  tbl$cells$setCell(3, 1, cellType="rowHeader", rawValue=5336)
  tbl$cells$setCell(3, 2, cellType="cell", rawValue="Orange")
  tbl$cells$setCell(3, 3, cellType="cell", rawValue=8)
  tbl$cells$setCell(3, 4, cellType="cell", rawValue=0.47)
  tbl$cells$setCell(4, 1, cellType="rowHeader", rawValue=5338)
  tbl$cells$setCell(4, 2, cellType="cell", rawValue="Banana")
  tbl$cells$setCell(4, 3, cellType="cell", rawValue=6)
  tbl$cells$setCell(4, 4, cellType="cell", rawValue=1.34)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
	str <- "Sale ID    Item  Quantity  Price  \n   5334   Apple         5   0.34  \n   5336  Orange         8   0.47  \n   5338  Banana         6   1.34  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("manipulation 1", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellType="rowHeader", rawValues=c(5334, 5336, 5338))
  tbl$cells$setColumn(2, cellType="cell", rawValues=c("Apple", "Orange", "Banana"))
  tbl$cells$setColumn(3, cellType="cell", rawValues=c(5, 8, 6))
  tbl$cells$setColumn(4, cellType="cell", rawValues=c(0.34, 0.47, 1.34))
  tbl$cells$insertColumn(3)
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Variety")
  tbl$cells$setColumn(3, rawValues=c("Gala", "Jaffa", "Yellow"))
  tbl$cells$insertRow(3)
  tbl$cells$setRow(3, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5335, "Pear", "Marit", 2, 0.89))
  tbl$cells$insertRow(5)
  tbl$cells$setRow(5, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5337, "Plum", "Sweet", 5, 1.59))
  tbl$cells$setCell(1, 6, cellType="columnHeader", rawValue="Total")
  qty <- tbl$cells$getColumnValues(4)
  price <- tbl$cells$getColumnValues(5)
  total <- qty * price
  tbl$cells$setColumn(6, cellType="cell", rawValues=total)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
	str <- "Sale ID    Item  Variety  Quantity  Price  Total  \n   5334   Apple     Gala         5   0.34    1.7  \n   5335    Pear    Marit         2   0.89   1.78  \n   5336  Orange    Jaffa         8   0.47   3.76  \n   5337    Plum    Sweet         5   1.59   7.95  \n   5338  Banana   Yellow         6   1.34   8.04  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Variety</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Price</td>\n    <td class=\"ColumnHeader\">Total</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">Gala</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">0.34</td>\n    <td class=\"Cell\">1.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5335</td>\n    <td class=\"Cell\">Pear</td>\n    <td class=\"Cell\">Marit</td>\n    <td class=\"Cell\">2</td>\n    <td class=\"Cell\">0.89</td>\n    <td class=\"Cell\">1.78</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">Jaffa</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">0.47</td>\n    <td class=\"Cell\">3.76</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5337</td>\n    <td class=\"Cell\">Plum</td>\n    <td class=\"Cell\">Sweet</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1.59</td>\n    <td class=\"Cell\">7.95</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">Yellow</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">1.34</td>\n    <td class=\"Cell\">8.04</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})


test_that("manipulation 2", {

  library(basictabler)
  tbl <- BasicTable$new()
  tbl$cells$setCell(1, 1, cellType="root", rawValue="Sale ID")
  tbl$cells$setCell(1, 2, cellType="columnHeader", rawValue="Item")
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Quantity")
  tbl$cells$setCell(1, 4, cellType="columnHeader", rawValue="Price")
  tbl$cells$setColumn(1, cellType="rowHeader", rawValues=c(5334, 5336, 5338))
  tbl$cells$setColumn(2, cellType="cell", rawValues=c("Apple", "Orange", "Banana"))
  tbl$cells$setColumn(3, cellType="cell", rawValues=c(5, 8, 6))
  tbl$cells$setColumn(4, cellType="cell", rawValues=c(0.34, 0.47, 1.34))
  tbl$cells$insertColumn(3)
  tbl$cells$setCell(1, 3, cellType="columnHeader", rawValue="Variety")
  tbl$cells$setColumn(3, rawValues=c("Gala", "Jaffa", "Yellow"))
  tbl$cells$insertRow(3)
  tbl$cells$setRow(3, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5335, "Pear", "Marit", 2, 0.89))
  tbl$cells$insertRow(5)
  tbl$cells$setRow(5, cellTypes=c("rowHeader", "cell", "cell", "cell", "cell"),
                   rawValues=list(5337, "Plum", "Sweet", 5, 1.59))
  tbl$cells$setCell(1, 6, cellType="columnHeader", rawValue="Total")
  qty <- tbl$cells$getColumnValues(4)
  price <- tbl$cells$getColumnValues(5)
  total <- qty * price
  tbl$cells$setColumn(6, cellType="cell", rawValues=total)
  tbl$cells$deleteColumn(5)
  tbl$cells$deleteRow(3)
  # tbl$renderTable()
  # prepStr(tbl$print(asCharacter=TRUE), "str")
  # prepStr(as.character(tbl$getHtml()))
	str <- "Sale ID    Item  Variety  Quantity  Total  \n   5334   Apple     Gala         5    1.7  \n   5336  Orange    Jaffa         8   3.76  \n   5337    Plum    Sweet         5   7.95  \n   5338  Banana   Yellow         6   8.04  "
	html <- "<table class=\"Table\">\n  <tr>\n    <td class=\"RowHeader\">Sale ID</td>\n    <td class=\"ColumnHeader\">Item</td>\n    <td class=\"ColumnHeader\">Variety</td>\n    <td class=\"ColumnHeader\">Quantity</td>\n    <td class=\"ColumnHeader\">Total</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5334</td>\n    <td class=\"Cell\">Apple</td>\n    <td class=\"Cell\">Gala</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">1.7</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5336</td>\n    <td class=\"Cell\">Orange</td>\n    <td class=\"Cell\">Jaffa</td>\n    <td class=\"Cell\">8</td>\n    <td class=\"Cell\">3.76</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5337</td>\n    <td class=\"Cell\">Plum</td>\n    <td class=\"Cell\">Sweet</td>\n    <td class=\"Cell\">5</td>\n    <td class=\"Cell\">7.95</td>\n  </tr>\n  <tr>\n    <td class=\"RowHeader\">5338</td>\n    <td class=\"Cell\">Banana</td>\n    <td class=\"Cell\">Yellow</td>\n    <td class=\"Cell\">6</td>\n    <td class=\"Cell\">8.04</td>\n  </tr>\n</table>"

	expect_identical(tbl$print(asCharacter=TRUE), str)
  expect_identical(as.character(tbl$getHtml()), html)
})
