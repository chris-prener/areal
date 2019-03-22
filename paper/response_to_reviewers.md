We would like to thank both @edzer and @sjsrey as well as @lheagy for the feedback and the opportunity to revise both the software and the manuscript.

## Reviewer 1 - @edzer
* The pdf of the paper shows R code on page 2, but this code is hidden where it runs off the page at the side.
    * The example has been changed to return a tibble object rather than an `sf` object. A new line was added below the example that clarifies the `sf` functionality as well.

* The manuscript claims it provides functions that "validate data suitability for areal interpolation", but does not explain what this validation does. What does it do?
    * A description of the validation process has been added to the manuscript in the section titled **The `areal` R package**.

* given that the package does little more than sf::st_interpolate_aw, and given that that function was available first, the added value of the packages would become more clear when it would mention the name of that function (rather than refer to "the existing functionality in sf"),
    * A direct reference to `sf::st_interpolate_aw` in the first paragraph of the section titled **The `areal` R package**.

* show in a side-by-side comparison that areal::aw_interpolate and sf::st_interpolate_aw give identical results for an intensive an an extensive variable

* give an example where areal::aw_interpolate does something different when there is a difference in areas, and show how large the effect of doing this is

* the correct reference for Pebesma, E. (2018) is found here
    * This has been corrected

* change the wording of "black box", as this is not a black box. Closed source software is a black box

* the areal-weighted-interpolation vignette mentions "Spatially intensive operations are used when the data to be interpolated are a ratio." I'm afraid it is not this simple: a counter example is is CO2 emissions measured in tons/year: an extensive variable when interpolated spatially.

* Additionally, @edzer provided [feedback](https://github.com/slu-openGIS/areal/issues/18) about two packages listed as dependencies that may not be necessary.
    * Both `lwgeom` and `tibble` have been removed as dependencies
