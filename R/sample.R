
#' Generate a sample basic table.
#'
#' The \code{basictablerSample} function generates a sample basic table.
#'
#' @import dplyr
#' @export
#' @return Example basic table describing the performance of trains run by
#'   different train operating companies in Birmingham (UK).
#' @examples
#' # Generate sample table.
#' basictablerSample()

basictablerSample <- function(value) {
  tocdata <- dplyr::group_by(bhmsummary, TOC)
  tocdata <- dplyr::summarise(tocdata, OnTimeArrivals=sum(OnTimeArrivals),
                              OnTimeDepartures=sum(OnTimeDepartures), TotalTrains=sum(TrainCount))
  tocdata <- dplyr::ungroup(tocdata)
  tocdata <- dplyr::mutate(tocdata, OnTimeArrivalPercent=OnTimeArrivals/TotalTrains*100,
                           OnTimeDeparturePercent=OnTimeDepartures/TotalTrains*100)
  tocsummary <- dplyr::arrange(tocdata, TOC)

  # To specify formatting, a list is created which contains one element for each column in
  # the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
  # The values in the first column in the data frame won't be formatted since NULL has been specified.
  # The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
  # The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
  columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

  # render the table directly as a html widget
  btbl <- qtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
               explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                       "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
               columnFormats=columnFormats)

  return(btbl)
}
