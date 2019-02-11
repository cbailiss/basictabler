basictabler 0.2.0.9000
================

Overview
--------

This release includes:

* TBC
* TBC
* TBC

Breaking Changes
----------------

TBC

Improvements
----------------

TBC

Bug Fixes
----------------

TBC

Upcoming Changes
----------------

TBC


basictabler 0.2.0
================

Overview
--------

This release includes:

* Table cell merging
* Many small styling improvements
* More styling information and examples in the vignettes

Breaking Changes
----------------

The default value of the `specifyCellsAsList` argument in the `tbl$getCells()` function has been changed to `TRUE` (previously `FALSE`) since this argument usage is more intuative and is consistent with the `pt$getCells()` function in the `pivottabler` package. 

Improvements
----------------

* Ability to merge table cells.  See the "Working with Cells" vignette for an example.
* Specifying the styling/formatting of individual cells/groups of cells is now simpler using the `tbl$setStyling()` function.  See the "Styling" vignette and the "Finding and Formatting" vignette for more details and examples. 
* Specifying styling/formatting when creating tables using the `qtbl()` and `qhtbl()` functions is now possible.  See the "Introduction" vignette for a list of parameters for these functions.  See the "Styling" vignette for more examples. 
* Specifying styling/formatting when creating tables from a data frame or row-by-row, column-by-column and cell-by-cell is now possible.  Again, see the "Styling" vignette and the "Finding and Formatting" vignette for more details and examples.
* A more detailed explanation of styling rules has been added to the "Styling" vignette.


basictabler 0.1.1
================

Bug Fixes
----------------

Tiny change to correct the index entry of the second vignette.


basictabler 0.1.0
================

Initial version.


Earlier versions
================

No versions prior to 0.1.0 were released.
