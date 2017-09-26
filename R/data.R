#' A Summary of Birmingham Trains, Dec 2016-Feb 2017.
#'
#' A dataset summarising all of the trains that either
#' originated at, passed through or terminated at
#' Birmingham New Street railway station in the UK
#' between 1st December 2016 and 28th February 2017
#' inclusive.
#'
#' @format A data frame with 4839 rows and 14 variables:
#' \describe{
#'   \item{Status}{Train status: A=Active, C=Cancelled, R=Reinstated}
#'   \item{TOC}{Train operating company}
#'   \item{TrainCategory}{Express Passenger or Ordinary Passenger}
#'   \item{PowerType}{DMU=Diesel Multiple Unit, EMU=Electrical Multiple Unit,
#'   HST=High Speed Train}
#'   \item{SchedSpeedMPH}{Scheduled maximum speed (in miles per hour)}
#'   \item{GbttMonth}{The month of the train in the Great Britain Train Timetable (GBTT)}
#'   \item{GbttWeekDate}{The week of the train in the Great Britain Train Timetable (GBTT)}
#'   \item{Origin}{3-letter code denoting the scheduled origin of the train}
#'   \item{Destination}{3-letter code denoting the scheduled destination of the train}
#'   \item{TrainCount}{The number of scheduled trains}
#'   \item{OnTimeArrivals}{The number of trains that arrived in Birmingham on time}
#'   \item{OnTimeDepartures}{The number of trains that departed Birmingham on time}
#'   \item{TotalArrivalDelayMinutes}{The total number of delay minutes for arrivals}
#'   \item{TotalDepartureDelayMinutes}{The total number of delay minutes for departures}
#' }
#' @source \url{http://www.recenttraintimes.co.uk/}
"bhmsummary"



#' Train Stations
#'
#' A reference dataset listing the codes, names and locations
#' of trains stations in Great Britain.
#'
#' @format A data frame with 2568 rows and  7 variables:
#' \describe{
#'   \item{CrsCode}{3-letter code for the station}
#'   \item{StationName}{The name of the station}
#'   \item{OsEasting}{The UK Ordnance Survey Easting coordinate for the station}
#'   \item{OsNorthing}{The UK Ordnance Survey Northing coordinate for the
#'   station}
#'   \item{GridReference}{Grid reference for the station}
#'   \item{Latitude}{Latitude of the station location}
#'   \item{Longitude}{Longitude of the station location}
#' }
#' @source \url{http://www.recenttraintimes.co.uk/}
"trainstations"
