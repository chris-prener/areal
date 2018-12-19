# create map for README, areal vignette

library(areal)
library(sf)
library(ggplot2)
library(cowplot)
library(prener)
library(here)

p1 <- ggplot() +
  geom_sf(data = ar_stl_race, fill = "#000000", color = "#ffffff") +
  labs(
    title = "Census Tract Features",
    subtitle = "St. Louis, MO"
  ) +
  cp_sequoiaTheme(base_size = 14, background = "white", map = TRUE) +
  theme(legend.position="none")

p2 <- ggplot() +
  geom_sf(data = ar_stl_wards, fill = "#000000", color = "#ffffff") +
  labs(
    title = "2010 Ward Features",
    subtitle = "St. Louis, MO"
  ) +
  cp_sequoiaTheme(base_size = 14, background = "white", map = TRUE) +
  theme(legend.position="none")

theme_set(theme_cowplot(font_family = "sans", font_size = 14))

exampleMap <- plot_grid(p1, p2, labels = c("(A)", "(B)"))

ggplot2::ggsave(filename = here("man", "figures", "featureMap.png"), exampleMap,
       width = cp_points(800, units = "in"), height = cp_points(450, units = "in"), dpi = 72)

intersect <- st_intersection(ar_stl_race, ar_stl_wards)

p3 <- ggplot() +
  geom_sf(data = intersect, fill = "#000000", color = "#ffffff") +
  labs(
    title = "Intersected Features",
    subtitle = "St. Louis, MO"
  ) +
  cp_sequoiaTheme(base_size = 14, background = "white", map = TRUE) +
  theme(legend.position="none")

ggplot2::ggsave(filename = here("man", "figures", "intersectMap.png"), p3,
                width = cp_points(500, units = "in"), height = cp_points(450, units = "in"), dpi = 72)

p4 <- ggplot() +
  geom_sf(data = ar_stl_race, fill = "#ff0000", color = "#000000") +
  geom_sf(data = ar_stl_wardsClipped, fill = "#ffffff", color = "#000000") +
  labs(
    title = "Wards Clipped / Tracts Overlap",
    subtitle = "St. Louis, MO"
  ) +
  cp_sequoiaTheme(base_size = 14, background = "white", map = TRUE) +
  theme(legend.position="none")

ggplot2::ggsave(filename = here("man", "figures", "overlapMap.png"), p4,
                width = cp_points(500, units = "in"), height = cp_points(450, units = "in"), dpi = 72)

