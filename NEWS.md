basictabler vNext
================

Improvements
----------------

Tables can now be exported to a wider variety of file formats using the flextabler package.  Supported formats including Microsoft Word, Microsoft PowerPoint and PDF.  See the Outputs vignette for more details. 

It is now easier to format the borders for specific cells.  See the "Formatting cell borders for specific cells" section of the Styling vignette for more details.

Bug Fixes
----------------

Resolved error "Error in columnValues[r - rowOffset] <- v : replacement has length zero" that sometimes occurred when executing bt$asDataFrame().


basictabler 1.0.1
================

This release includes one small bug fix:  Tables exported to an Excel file would result in blank cells in the Excel worksheet for table cells which had a formatted value but no raw value (i.e. a raw value of `NULL`).  This can occur when exporting pivot tables from the `pivottabler` package for the row/column headings of totals and for row/column headings when multiple calculations are specified in the pivot table.

The `bt$writeToExcelWorksheet()` function has new parameter `useFormattedValueIfRawValueIsNull` which has a default value of `TRUE` to automatically use the formatted value if the raw value is `NULL`.

If required, the previous behaviour can be restored by specifying `useFormattedValueIfRawValueIsNull=FALSE`.


basictabler 1.0.0
================

This release includes three breaking changes and numerous small enhancements across various parts of the package.

Breaking Changes
----------------

**Argument order changes**

The order of the arguments for functions `qtbl()`, `qhtbl()` and `tbl$addData()` has changed.  Users relying on the order of arguments may need to update their code.  All existing arguments have retained the same name - so users calling these functions using the argument names will be unaffected.

**Change of default behaviour in `tbl$findCells()`**

Previously the `rowNumbers` and `columnNumbers` arguments of `tbl$findCells()` were applied in combination e.g. specifying rowNumbers=2:3 and columnNumbers=5:6 previously matched only those cells satisfying both the rowNumbers criteria **AND** the columnNumbers criteria, which is cells (2, 5), (2, 6), (3, 5) and (3, 6) - and not for example (2, 4).

From v1.0.0 of the package, the `rowNumbers` and `columnNumbers` arguments are by default applied independently, e.g. specifying rowNumbers=2:3 and columnNumbers=5:6 will match all cells in rows 2 and 3 and all cells in columns 5 and 6, i.e. cells must match either the rowNumbers criteria **OR** the columnNumbers criteria.

The previous behaviour can be restored using the `tbl$findCells()` argument `rowColumnMatchMode="combinations"`.

This change makes the behaviour of `tbl$getCells()` and `tbl$findCells()` consistent.  It also makes the behaviour consistent with the same functions in the `pivottabler` package.

**stringsAsFactors in R 4.0.x and 4.1.x**

From R 4.1.0, the default value of the `stringsAsFactors` argument in `tbl$asDataFrame()` changes to `FALSE` due to the deprecation of `default.stringsAsFactors()`.  When the package is used on versions of R < 4.1.0, the package behaviour is unchanged.  When the package used on R 4.0.x versions, a warning message is displayed about the change in future behaviour. 

Documentation Changes
---------------------

* The code documentation for `basictabler` has been re-written to use the R6 documentation capabilities in `roxygen2`.  As a result the documentation is now more detailed than in previous versions, e.g. arguments in object method calls are now properly documented.  Nonetheless, the easiest way to learn the package is using the vignettes. 
* The package documentation is now also available at:  http://www.basictabler.org.uk/

Improvements
----------------

* The `qtbl()`, `qhtbl()` and `tbl$addData()` functions gain additional parameters `numberOfColumnsAsRowHeaders` and `columnCellTypes` that provide additional options for specifying cell types when creating tables from data frames. 
* The `tbl$mergecells()` method gains new parameters `rowNumbers` and `columnNumbers` to allow vectors to be used to specify the range of cells to be merged.
* `pt$setStyling()` gains new integer/numeric vector arguments `rowNumbers` and/or `columnNumbers` to constrain the cells styling is applied to.  It is now also possible to specify only a set of row numbers and then all cells in those rows will be styled (and similarly for column numbers).  Previously, if only row numbers or only column numbers were specified, then no cells would be styled.  Users who require the old logic can specify the argument `compatibility=list(legacySetStylingRowColumnNumbers=TRUE)` when calling `PivotTable$new()`.
* `pt$setStyling()` also gains new arguments `cellType` and `visible` to allow these cell properties to be easily specified for ranges of cells.  See the styling vignette for more details.  
* New method `pt$mapStyling()` simplifies applying styling to cells based on cell value, e.g. banding by value into different colours or colour gradients.  See the "Styling" vignette for details plus the "Finding and Formatting" vignette for an example.
* `tbl$getCells()` gains a new `excludeEmptyCells` argument making it easier to exclude cells containing no values.  See the "Finding and Formatting" vignette for details.
* `tbl$getCells()` also gains a new `matchMode` argument making it easier to retrieve cells based on combinations of row and column criteria.  See the "Finding and Formatting" vignette for details.
* `pt$findCells()` gain new arguments `rowNumbers`, `columnNumbers`, `cellCoordinates`, `cells` and `rowColumnMatchMode` to restrict the cell search based on combinations of row, column and cell criteria.  See the "Finding and Formatting" vignette for details.
* `pt$findCells()` also gains new argument `valueRanges` to enable more granular logic to be specified when matching cell values.  See the "Finding and Formatting" vignette for details.
* When using a simple theme (specified as a list) to style a pivot table, it is now possible to specify a font size.  
* New property `tbl$allCells` provides a simple way to retrieve a list of all cells in the table.
* Additional arguments can now be passed to custom calculation functions using the `calcFuncArgs` argument.  See the documentation of the `BasicTable$addData()`, `BasicTable$addMatrix()`, `TableCells$setRow()` and `TableCells$setColumn()` functions for details. 


basictabler 0.3.1
================

This is a maintenance release that fixes a couple of compatibility issues around the new default of `stringsAsFactors=FALSE` in data.frame() in the upcoming R 4.0.0 release.


basictabler 0.3.0
================

Overview
--------

This release includes two small potentially breaking changes to the HTML generated by the package and one bug fix.

Breaking Changes
----------------

**Changes to table header cells in HTML**

When generating HTML, previous versions of the package rendered all cells using the html **td** element.  More correctly, header cells should be rendered using the **th** element.  

This behaviour also causes compatibility issues with the `pivottabler` package, which already renders header cells using **th** - i.e. pivot tables converted to basic tables were rendered differently by `basictabler` compared to `pivottabler`.

Starting from this version of `basictabler`, header cells are rendered as **th** elements.  In most cases, this will make no difference to the visual appearance of the table, however it may cause issues for users who require the previous behaviour.  The previous behaviour is still available by specifying `compatibility=list(headerCellsAsTD=TRUE)` as an argument when creating the table, either in `BasicTable$new()` or one of the quick table functions such as `qtbl()`.

Originally reported by @rickwargo (thanks!).

**Changes to rowspan and colspan attributes in HTML**

When generating HTML, v0.2.0 would always generate rowspan and colspan attributes for merged table cells, even if the number of rows or columns being spanned was only one.  Starting with v0.3.0, rowspan and colspan attributes are only generated where the number of rows or columns being spanned is greater than one.  This should make no difference to the visual appearance of the table, however it may cause issues for users who require the previous behaviour.  The previous behaviour is still available by specifying `compatibility=list(explicitHeaderSpansOfOne=TRUE)` as an argument when creating the table, either in `BasicTable$new()` or one of the quick table functions such as `qtbl()`.

Bug Fixes
----------------

Calling `tbl$setStyling()` on the same cell multiple times now succeeds (previously failed with error).  Originally reported by @palatinuse (thanks!).


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

The default value of the `specifyCellsAsList` argument in the `tbl$getCells()` function has been changed to `TRUE` (previously `FALSE`) since this argument usage is more intuitive and is consistent with the `pt$getCells()` function in the `pivottabler` package. 

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
