## Release summary
 This is the initial CRAN submission.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on Travis CI), R-release, R-oldrel, R-devel
* macOS (on Travis CI), R-release, R-oldrel
* windows i386 (on Appveyor), R-release, R-oldrel, R-devel
* windows x64 (on Appveyor), R-release, R-oldrel, R-devel
* winbuilder, R-release, R-oldrel, R-devel

* r-hub not used because it lacks dependencies needed to build `sf` and `lwgeom` on Debian

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local checks or on Travis CI/Appveyor.

On devtools::release()'s R CMD check and all winbuilder checks, we get one NOTE:

  * checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Christopher Prener <chris.prener@slu.edu>'
  
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    pipeable (10:16)
    tidyverse (13:54)
    workflow (12:41)

All three words are spelled correctly for our purposes.

When checked against R-oldrel on Winbuilder, we got an additional NOTE:

  * checking DESCRIPTION meta-information ... NOTE
  Author field differs from that derived from Authors@R
    Author:    'Christopher Prener [aut, cre] (<https://orcid.org/0000-0002-4310-9888>), Charlie Revord [aut]'
    Authors@R: 'Christopher Prener [aut, cre] (0000-0002-4310-9888), Charlie Revord [aut]'

This note only appears in this testing enviornment, and the `Authors@R` field in `DESCRIPTION` appears correctly constructed. 

## Reverse dependencies
Not applicable.
