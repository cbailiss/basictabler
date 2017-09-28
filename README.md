basictabler
================

[![Build Status](https://travis-ci.org/cbailiss/basictabler.svg?branch=master)](https://travis-ci.org/cbailiss/basictabler) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/basictabler)](https://cran.r-project.org/package=basictabler)

The `basictabler` package enables basic tables to be created and rendered/exported with just a few lines of R.

The `basictabler` package:

-   provides an easy way of creating basic tables.
-   Provide an especially easy way of generating tables from data frames and matrices.
-   Provide styling options so the basic tables can be themed/branded as needed.

The tables are rendered as htmlwidgets or plain text. The HTML/text can be exported for use outside of R.

The tables can also be exported to Excel, including the styling/formatting.

`basictabler` is a companion package to the `pivottabler` package. `pivottabler` is focussed on generating pivot tables and can aggregate data. `basictabler` does not aggregate data but offers more control of table structure.

### Installation

You can install:

-   the latest released version from CRAN with

``` r
install.packages("basictabler")
```

-   the latest development version from github with

``` r
devtools::install_github("cbailiss/basictabler", build_vignettes = TRUE)
```

### Example

#### Trivial Example

Creating a tiny HTML table from a data frame and immediately rendering it as a htmlwidget:

``` r
library(basictabler)
qhtbl(data.frame(a=1:2, b=3:4))
```

#### Another Example

Creating a table from a data frame, specifying column names and value formats:

``` r
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

# To specify formatting, a list is created which contains one element for each column in 
# the data frame, i.e. tocsummary contains six columns so the columnFormats list has six elements.
# The values in the first column in the data frame won't be formatted since NULL has been specified.
# The values in the 2nd, 3rd and 4th columns will be formatted using format(value, big.mark=",")
# The values in the 5th and 6th columns will be formatted using sprintf(value, "%.1f")
columnFormats=list(NULL, list(big.mark=","), list(big.mark=","), list(big.mark=","), "%.1f", "%.1f")

# render the table directly as a html widget
qhtbl(tocsummary, firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c("TOC", "On-Time Arrivals", "On-Time Departures",
                                    "Total Trains", "On-Time Arrival %", "On-Time Departure %"),
            columnFormats=columnFormats)
```

![<http://cbailiss.me.uk/basictablerreadmeimgs/example1.png>](http://cbailiss.me.uk/basictablerreadmeimgs/example1.png)

Tables can be further manipulated once created, including adding/removing cells/rows/columns and specifying styling and formatting.

It is also possible to create tables row-by-row, column-by-column and/or cell-by-cell.

### More Information

See the package vignettes for more information:

``` r
# to see a list of available package vignettes:
vignette(package="basictabler")
# to open a specific vignette
vignette(topic="v01-introduction", package="basictabler")
```

The vignettes can also be read on CRAN at: <https://cran.r-project.org/package=basictabler>
