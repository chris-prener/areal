# create map for README, areal vignette

library(areal)
library(ggplot2)
library(cowplot)
library(viridis)
library(prener)
library(here)

result <- aw_interpolate(ar_stl_wards, tid = WARD, source = ar_stl_race, sid = "GEOID",
                         weight = "sum", output = "sf", extensive = "TOTAL_E")

p1 <- ggplot() +
  geom_sf(ar_stl_race, mapping = aes(fill = TOTAL_E)) +
  scale_fill_viridis() +
  labs(
    title = "Total Population per Tract",
    subtitle = "St. Louis, MO (2017)"
  ) +
  cp_sequoiaTheme(base_size = 18, background = "white", map = TRUE) +
  theme(legend.key.size = unit(1, units="cm"))

p2 <- ggplot() +
  geom_sf(result, mapping = aes(fill = TOTAL_E)) +
  scale_fill_viridis(option="plasma") +
  labs(
    title = "Total Population per Ward",
    subtitle = "St. Louis, MO (2017)"
  ) +
  cp_sequoiaTheme(base_size = 18, background = "white", map = TRUE) +
  theme(legend.key.size = unit(1, units="cm"))

theme_set(theme_cowplot(font_family = "sans"))

exampleMap <- plot_grid(p1, p2, labels = c("(A)", "(B)"))

ggsave(filename = here("man", "figures", "exampleMap.png"), exampleMap,
       width = cp_points(800, units = "in"), height = cp_points(450, units = "in"), dpi = 72)
