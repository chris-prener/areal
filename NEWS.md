# areal 0.1.7

* Address situations where the `geometry` column has a different name. In prior versions, it was renamed. Now, we use an approach that does not require a specific `geometry` column name but should work regardless of what its name is.
* Address CRAN warning with vignette rendering.

# areal 0.1.6

* Prepare `areal` for upcoming `dplyr` release
* Address bug with geometry collections that could cause errors in `aw_intersect()`
* Resolve various warnings during unit tests
* Change minimum version of R to 3.4
* Update travis and appveyor builds

# areal 0.1.5

* Add *JOSS* badges to the `README` and `pkgdown` sites
* Add paper description to the `README` and `pkgdown` sites
* Update `CITATION` file to match *JOSS* publication information
* Update package hex logo
* Remove unit testing that was cuasing problems on the noLD CRAN check of the current accepted version of the package
* **No changes to the software functionality**

# areal 0.1.4.4

* Finalize *JOSS* manuscript
* Rebuild pkgdown site
* **No changes to the software functionality**

# areal 0.1.4.3

* Remove documentation for non-exported functions
* Rebuild pkgdown site
* **No changes to the software functionality**

# areal 0.1.4.2

* Fix formatting issue with *JOSS* manuscript
* **No changes to the software functionality**

# areal 0.1.4.1

* Finalize *Journal of Open Source Software* manuscript
* **No changes to the software functionality**

# areal 0.1.4

* Fix issue where mixed interpolations with multiple extensive and intensive variables would get incorrect output containing two copies of the extensive results.
* Patch fix to [Issue 6](https://github.com/slu-openGIS/areal/issues/6) so that data not requiring the geometry collection fix do not get processed through that code, improving performance for those interpolations.

* Updated draft of JOSS paper manuscript along with appendix code and response to reviewers added in `paper/`.

# areal 0.1.3

* Fix [Issue 6](https://github.com/slu-openGIS/areal/issues/6) - edge case where `st_intersection` creates a geometry collection
* Fix [Issue 7](https://github.com/slu-openGIS/areal/issues/7) - interpolation returns `NA` values when `tid` and `sid` are the same variable name (e.g. both are `GEOID`)
* Fix [Issue 14](https://github.com/slu-openGIS/areal/issues/14) - interpolation fails when the `sf` geometry column is not named `geometry` - now renamed on the fly if this is not the case
* Fix [Issue 16](https://github.com/slu-openGIS/areal/issues/16) - tibble output missing other output variables

* Full draft of JOSS paper manuscript added in `paper/`

# areal 0.1.2

* Add CRAN installation instructions to readme and `pkgdown` site

# areal 0.1.1

* Added vignette titles to replace placeholders
* Added examples to all exported functions
* Spell check with `devtools::spell_check()` and RStudio's spell check functionality for `.Rmd` files
* Checks against winbuilder

# areal 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Develop areal weighted interpolation functionality (`aw_` functions)
* Add core example data sets (`ar_` data)
* Implement unit testing with both Travis and Appveyor as well as Code Coverage tracking
* Add `.github` community files
* Add `LICENSE` and `cran-comments.md`
* Add vignettes for getting started (`areal`), preparing data (`data-preparation`), and areal weighted interpolation (`areal-weighted-interpolation`) - these are in *draft* for this release.
* Add pkgdown site functionality
