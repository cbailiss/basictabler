
## Background

This release addresses the current test failures on CRAN for R-devel.  These failures were caused by the change to stringsAsFactors=FALSE as the default in data.frame() in R-devel for R 4.0.0. 

This package is written entirely in R with no external dependencies/components other than the packages listed in the DESCRIPTION file.
Development approach is aligned with practices described in:
http://r-pkgs.had.co.nz/

## Test environments

* local OS (windows) install, R 3.6.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2.
* win-builder, R-devel
* R-hub, three tests:
  * Windows Server 2008 R2 SP1, R-devel 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

### Local R CMD check results

0 errors | 0 warnings | 0 notes

### Travis-CI R CMD check results

0 errors | 0 warnings | 0 notes

### win-builder check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None.
