library(basemapR)
library(dplyr)


ggplot() +
  base_map(st_bbox(shape), increase_zoom = 2, basemap = 'positron') +
  geom_sf(data = shape, fill = NA)
