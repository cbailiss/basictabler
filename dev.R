
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
tbl$renderTable()



tbl$viewJSON()




# Testing adding and removing rows/columns/cells
# need to test adding cells within, just on the edge of and well outside the existing table

# tbl$cells$insertRow(4)
# tbl$cells$addCell(4, 3, rawValue=123.45, formattedValue="123.45")
# tbl$cells$addCell(4, 5, rawValue=323.45, formattedValue="323.45")
# tbl$renderTable()
#
# tbl$cells$deleteCell(4, 3)
# tbl$renderTable()
#
# tbl$cells$insertColumn(5)
# tbl$cells$addCell(1, 5, cellType="columnHeader", rawValue="New", formattedValue="New")
# tbl$cells$addCell(3, 5, rawValue=226, formattedValue="226")
# tbl$renderTable()
#
# tbl$cells$deleteRow(4)
# tbl$renderTable()
#
# tbl$cells$deleteColumn(5)
# tbl$renderTable()

# cell <- tbl$cells$getCell(3, 2)
# cell$visible <- FALSE
# tbl$renderTable()




# Testing getting cells

# highlight <- TableStyle$new(tbl, "cellHighlight", list("background-color"="#FFCC66"))
# cells <- tbl$getCells(specifyCellsAsList=TRUE, rowNumbers=c(2, 4), cellCoordinates=list(c(5,3)))
# lst <- lapply(cells, function(cell) {cell$style <- highlight})
# tbl$renderTable()



# Testing finding cells

# highlight <- TableStyle$new(tbl, "cellHighlight", list("background-color"="#FF00FF"))
# cells <- tbl$findCells(columnNumbers=5, minValue=30)
# lst <- lapply(cells, function(cell) {cell$style <- highlight})
# tbl$renderTable()



# As text (to console)

# tbl$print()



# Converting to a matrix

# m <- tbl$asMatrix(firstRowAsColumnNames=FALSE, rawValue=FALSE)
# m <- tbl$asMatrix(firstRowAsColumnNames=FALSE, rawValue=TRUE)
# m <- tbl$asMatrix(firstRowAsColumnNames=TRUE, rawValue=FALSE)
# m <- tbl$asMatrix(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE)


# Converting to a data frame

# df <- tbl$asDataFrame(firstRowAsColumnNames=FALSE, rawValue=FALSE)
# df <- tbl$asDataFrame(firstRowAsColumnNames=FALSE, rawValue=TRUE)
# df <- tbl$asDataFrame(firstRowAsColumnNames=TRUE, rawValue=FALSE)
# df <- tbl$asDataFrame(firstRowAsColumnNames=TRUE, rawValue=TRUE)
# df <- tbl$asDataFrame(firstRowAsColumnNames=TRUE, firstColumnAsRowNames=TRUE, rawValue=TRUE)



# Excel Export

# library(openxlsx)
# wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wb, "Data")
# tbl$writeToExcelWorksheet(wb=wb, wsName="Data",
#                          topRowNumber=1, leftMostColumnNumber=1,
#                          applyStyles=TRUE, mapStylesFromCSS=TRUE)
#
# saveWorkbook(wb, file="C:\\test.xlsx", overwrite = TRUE)
