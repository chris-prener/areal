# create combined data

devtools::load_all()

## create modified asthma data
aw_stl_asthma %>%
  dplyr::select(-STATEFP, -COUNTYFP, -TRACTCE, -NAMELSAD, -ALAND, -AWATER) %>%
  dplyr::mutate(ASTHMA2 = ASTHMA/2) -> asthma

## create combined data
### remove sf geometry
race <- aw_stl_race
sf::st_geometry(race) <- NULL

### create combined data
race %>%
  dplyr::select(GEOID, TOTAL_E, WHITE_E, BLACK_E) %>%
  dplyr::left_join(asthma, ., by = "GEOID") -> combinedData

usethis::use_data(combinedData, overwrite = TRUE)
