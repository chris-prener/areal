We would like to thank both @edzer and @sjsrey as well as @lheagy for the feedback and the opportunity to revise both the software and the manuscript. We believe that the manuscript has been greatly strengthened as a result of your collective feedback and are excited to resubmit it for your review.

## Reviewer 1 - @edzer
* The pdf of the paper shows R code on page 2, but this code is hidden where it runs off the page at the side.
    * The example has been changed to return a tibble object rather than an `sf` object. A new line was added below the example that clarifies the `sf` functionality as well.

* The manuscript claims it provides functions that "validate data suitability for areal interpolation", but does not explain what this validation does. What does it do?
    * A description of the validation process has been added to the manuscript in the section titled **The `areal` R package**.

* given that the package does little more than sf::st_interpolate_aw, and given that that function was available first, the added value of the packages would become more clear when it would mention the name of that function (rather than refer to "the existing functionality in sf"),
    * A direct reference to `sf::st_interpolate_aw` in the first paragraph of the section titled **The `areal` R package**.

* show in a side-by-side comparison that areal::aw_interpolate and sf::st_interpolate_aw give identical results for an intensive an extensive variable
    * This is included in the new **A Quick Comparison with sf** section with two figures, one for extensive interpolations and one for intensive interpolations.

* give an example where areal::aw_interpolate does something different when there is a difference in areas, and show how large the effect of doing this is
    * This is included in the new **A Quick Comparison with sf** section with a figure and a description of the difference as well as why one approach may be preferable over the other.

* the correct reference for Pebesma, E. (2018) is found here
    * This has been corrected

* change the wording of "black box", as this is not a black box. Closed source software is a black box
    * This has been changed to:

    > "for users who need to unpack the interpolation workflow"

* the areal-weighted-interpolation vignette mentions "Spatially intensive operations are used when the data to be interpolated are a ratio." I'm afraid it is not this simple: a counter example is is CO2 emissions measured in tons/year: an extensive variable when interpolated spatially.
    * This has been changed to:

    > "Spatially *intensive* operations are used when the data to be interpolated are, for example, a percentage or density value."

* Additionally, @edzer provided [feedback](https://github.com/slu-openGIS/areal/issues/18) about two packages listed as dependencies that may not be necessary.
    * Both `lwgeom` and `tibble` have been removed as dependencies

## Reviewer 2 - @sjsrey

* Scaling: the current examples are well crafted and illustrate the core functionality. At the same time n is fairly modest here and one wonders if these methods were going to be used for larger n problems, or applied iteratively over many cities , would there be any bottlenecks users should be aware of?
    * We have added benchmarks to the end a new section titled **Performance Notes** as well as a discussion of a special case that `areal` handles, which is when geometry collections are returned by `sf::st_intersection()`. The process for handling this special case is more resource intensive and results in longer processing times.

* The distinction between extensive and intensive variables is handled nicely. One case that might also be considered is the choice between interpolation of an intensive variable directly versus deriving the intensive estimates as a ratio of two extensive variables that have been estimated
    * Mindful of the short length of JOSS manuscripts, we have included a discussion of this in the vignette on areal weighted interpolation in the section entitled **Mixed Interpolations**.

* The weight argument names are a bit confusing.
    * Since the package is already on CRAN, we are reluctant to change the argument. In the paper, however, we have made two clarifications to address this. The first was to explicitly link the names for both weights to their descriptions in the second to last paragraph of the section **The areal R package**. They now read:

    > "...we offer a formula that matches the existing functionality in `sf`, which is based on the total area of the original source feature (specified with `weight = "total"`)."

    > "This uses a sum of the source feature areas remaining after the data are intersected as part of the spatial weight calculation process (specified with `weight = "sum"`)."

    We also have added a section titled **A Quick Comparison with sf** that illustrates the difference between these two approaches, and includes a brief discussion of the selection process between both in the second and third full paragraphs. It includes the following language:

    > "If we expect that our source and target data should overlap completely (but perhaps do not because of data quality issues), the `"sum"` approach will allocate all individuals into target features whereas `"total"`will not, instead allocating only a proportion of individuals relative to the overlap between the source and target features. In the example data provided in the `areal` package, this is the case - the Census tracts do not perfectly map onto the extent of the wards, and so the `"total"` approach is inappropriate."
