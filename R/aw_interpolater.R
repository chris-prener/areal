aw_interpolater <- function(source = source, target = target, intersection = intersection, sid = GEOID, tid = WARD10, value = value, areaVar = "geom_area", var = "geom_area", NewField = pop_new, initialVals = value) {
 intersection <- aw_intersect(source, target)
 intersection <- aw_sum_area(intersection)
 intersection <- aw_area_wght(intersection)
 intersection <- aw_new_field(intersection)
 intersection <- aw_field_new(intersection)
 out <- aw_aggregate(intersection)
 return(out)
}
