# comparisons with `sf`
library(areal)
library(dplyr)
library(gridExtra)
library(microbenchmark)
library(sf)

# areal package, spatially extensive using total
areal_exT <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
               weight = "total", output = "tibble", extensive = "TOTAL_E")

# areal package, spatially extensive using sum
areal_exS <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                            weight = "sum", output = "tibble", extensive = "TOTAL_E")

# areal package, spatially intensive
areal_in <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                            weight = "sum", output = "tibble", intensive = "ASTHMA")

# sf package, spatially extensive
sf_ex <- st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE)

# sf package, spatially intensive
sf_in <- st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE)

# compile results
areal_exS <- areal_exS %>%
  select(WARD, TOTAL_E) %>%
  rename(areal_exS = TOTAL_E)

areal_exT <- areal_exT %>%
  select(WARD, TOTAL_E) %>%
  rename(areal_exT = TOTAL_E)

sf_ex <- sf_ex %>%
  rename(sf_ex = TOTAL_E)
st_geometry(sf_ex) <- NULL

extensive <- left_join(sf_ex, areal_exT, by = c("Group.1" = "WARD")) %>%
  left_join(., areal_exS, by = c("Group.1" = "WARD")) %>%
  mutate(delta = areal_exT-areal_exS) %>%
  rename(Ward = Group.1) %>%
  as_tibble()

areal_in <- areal_in %>%
  select(WARD, ASTHMA) %>%
  rename(areal_in = ASTHMA)

sf_in <- sf_in %>%
  rename(sf_in = ASTHMA)
st_geometry(sf_in) <- NULL

intensive <- left_join(sf_in, areal_in, by = c("Group.1" = "WARD")) %>%
  rename(Ward = Group.1) %>%
  as_tibble()

extensiveSub <- extensive %>%
  filter(Ward >= 1 & Ward <= 10) %>%
  mutate(
    sf_ex = round(sf_ex, digits = 3),
    areal_exT = round(areal_exT, digits = 3),
    areal_exS = round(areal_exS, digits = 3),
    delta = round(delta, digits = 3)
  ) %>%
  rename(
    `sf` = sf_ex,
    `areal, total weight` = areal_exT,
    `areal, sum weight` = areal_exS
  )

intensiveSub <- intensive %>%
  filter(Ward >= 1 & Ward <= 10) %>%
  mutate(
    sf_in = round(sf_in, digits = 3),
    areal_in = round(areal_in, digits = 3)
  ) %>%
  rename(
    `sf` = sf_in,
    `areal` = areal_in
  )

png(filename = "paper/extensiveTable.png", width = 480, height = 300, bg = "white", type = "cairo-png")
grid.arrange(tableGrob(extensiveSub, rows = NULL), top = "Comparison of sf and areal Output\nSpatially Extensive Interpolation")
dev.off()

png(filename = "paper/intensiveTable.png", width = 480, height = 300, bg = "white", type = "cairo-png")
grid.arrange(tableGrob(intensiveSub, rows = NULL), top = "Comparison of sf and areal Output\nSpatially Intensive Interpolation")
dev.off()

# compare spatially extensive interpolations
microbenchmark(
  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = GEOID,
                 weight = "total", output = "tibble", extensive = "TOTAL_E"),
  suppressWarnings(st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE))
)

# Unit: milliseconds
# expr
# aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race,
#     sid = GEOID, weight = "total", output = "tibble", extensive = "TOTAL_E")
# st_interpolate_aw(ar_stl_race["TOTAL_E"], ar_stl_wards, extensive = TRUE)
#
# min       lq     mean   median       uq      max neval cld
# 257.2321 267.9513 288.0400 276.5045 296.2457 397.4150   100   b
# 235.2296 244.6525 269.2864 255.9796 276.8292 447.5478   100  a

# compare spatially intensive interpolations
microbenchmark(
  aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma, sid = GEOID,
                 weight = "sum", output = "tibble", intensive = "ASTHMA"),
  suppressWarnings(st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE))
)

# Unit: milliseconds
# expr
# aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_asthma,
#     id = GEOID, weight = "sum", output = "tibble", intensive = "ASTHMA")
# st_interpolate_aw(ar_stl_asthma["ASTHMA"], ar_stl_wards, extensive = FALSE)
#
# min       lq     mean   median       uq      max neval cld
# 246.3021 257.4800 290.9981 273.5521 310.5266 535.7888   100   b
# 229.3634 239.6297 263.6643 251.5276 275.0800 390.5386   100  a


library(tigris)
library(tidycensus)

moPop <- get_acs(geography = "county", variables = "B01003_001", output = "wide", state = 29, geometry = TRUE) %>%
  st_transform(crs = 26915) %>%
  select(GEOID, B01003_001E) %>%
  rename(totalPop = B01003_001E)

moBlockGroups <- block_groups(state = 29, class = "sf") %>%
  st_transform(crs = 26915) %>%
  select(GEOID)

bgPop <- aw_interpolate(moBlockGroups, tid = GEOID, source = moPop, sid = GEOID,
               weight = "sum", output = "tibble", intensive = "totalPop")

microbenchmark(
  aw_interpolate(moBlockGroups, tid = GEOID, source = moPop, sid = GEOID,
                 weight = "sum", output = "tibble", intensive = "totalPop")
)
