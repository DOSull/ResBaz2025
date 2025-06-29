## ------------------
# Making maps in R - workshop © 2025 
# by David O'Sullivan is licensed under 
# CC-BY-NC-SA 4.0
# https://creativecommons.org/licenses/by-nc-sa/4.0/


## ------------------
library(tidyverse)
library(dplyr)
library(palmerpenguins)
data("penguins")
penguins <- penguins |> drop_na()

library(ggplot2)
theme_set(theme_minimal())


## ------------------
ggplot(penguins) +
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm))


## ------------------
ggplot(penguins) + 
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm, 
                 colour = species))


## ------------------
ggplot(penguins) +
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm, 
                 colour = species, size = body_mass_g), 
             alpha = 0.6)


## ------------------
ggplot(penguins) +
  geom_histogram(aes(x = flipper_length_mm), binwidth = 10, 
                 colour = "black", fill = "lightgrey", lwd = 0.35)


## ------------------
ggplot(penguins) +
  geom_boxplot(aes(x = flipper_length_mm, y = species, fill = species))


## ------------------
ggplot(penguins) +
  geom_density(aes(x = flipper_length_mm, group = species, fill = species), 
               alpha = 0.5, lwd = 0)


## ------------------
ggplot(penguins) +
  geom_point(aes(x = flipper_length_mm, y = bill_length_mm, 
                 colour = species), size = 1, shape = 3) +
  geom_density_2d(aes(x = flipper_length_mm, y = bill_length_mm, 
                      group = species, colour = species))


## ------------------
library(sf)

ak_areas       <- st_read("data/ak-2006-ethnicity-and-tb.gpkg")
ak_major_roads <- st_read("data/ak-major-roads.gpkg")
ak_tb_cases    <- st_read("data/ak-tb-cases.gpkg")


## ------------------
ak_areas


## ------------------
ak_areas |> 
  select(tb_cases, tb_per_100k) |>
  filter(tb_per_100k > 100) 


## ------------------
ak_areas |> select(-OBJECTID, -AU_NO) |> plot()


## ------------------
ggplot(ak_areas) +
  geom_sf(aes(geometry = geom))


## ------------------
ggplot(ak_areas) +
  geom_sf()


## ------------------
ggplot(ak_areas) +
  geom_sf() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())


## ------------------
ggplot(ak_areas) +
  geom_sf() +
  theme_void()


## ------------------
theme_set(theme_void())


## ------------------
ggplot() +
  geom_sf(data = ak_areas) +
  geom_sf(data = ak_major_roads) + 
  geom_sf(data = ak_tb_cases)


## ------------------
ggplot() +
  geom_sf(data = ak_areas, colour = "white", fill = "grey") +
  geom_sf(data = ak_major_roads, colour = "red") + 
  geom_sf(data = ak_tb_cases, colour = "black", shape = 4)


## ------------------
ak_context <- st_read("data/ak-context.gpkg")


## ------------------
ggplot() +
  geom_sf(data = ak_context) +
  geom_sf(data = ak_areas, fill = "darkgrey")


## ------------------
bb <- st_bbox(ak_areas)
bb


## ------------------
ggplot() +
  geom_sf(data = ak_context, lwd = 0) +
  geom_sf(data = ak_areas, fill = "grey", colour = "white") +
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))


## ------------------
ggplot() +
  geom_sf(data = ak_context, lwd = 0) +
  geom_sf(data = ak_areas, fill = "grey", colour = "white") +
  coord_sf(xlim = c(bb[1] - 2000, bb[3] + 2000), 
           ylim = c(bb[2] - 2000, bb[4] + 2000))


## ------------------
ggplot() +
  geom_sf(data = ak_context, lwd = 0) +
  geom_sf(data = ak_areas, fill = "grey", colour = "white") +
  coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]),
           expand = FALSE)


## ------------------
ggplot(ak_areas) +
  geom_sf(aes(fill = nz_european), colour = "white")


## ------------------
library(cols4all)


## ------------------
ggplot(ak_areas) +
  geom_sf(aes(fill = nz_european), colour = "white") +
  scale_fill_continuous_c4a_seq(palette = "viridis")


## ------------------
ak_area_centroids <- ak_areas |> 
  st_centroid() |>
  filter(tb_cases > 0)


## ------------------
ggplot(ak_areas) +
  geom_sf() + 
  geom_sf(data = ak_area_centroids, aes(size = tb_cases),
          colour = "red", alpha = 0.5) 


## ------------------
library(ggspatial)


## ------------------
ggplot(ak_areas) +
  geom_sf() +
  annotation_north_arrow(location = "tl") +
  annotation_scale(location = "bl")


## ------------------
ggplot(ak_areas) +
  geom_sf() +
  geom_sf_text(aes(label = suburb), check_overlap = TRUE)


## ------------------
ak_areas_long <- ak_areas |>
  pivot_longer(cols = c("nz_european", "maori", "pacific", "asian"))
ak_areas_long


## ------------------
ggplot(ak_areas_long) + 
  geom_sf(aes(fill = value)) +
  scale_fill_continuous_c4a_seq(palette = "brewer.reds") +
  facet_wrap( ~ name)


## ------------------
library(terra)

hillshade <- rast("data/ak-hillshade.tif")
xyz <- hillshade |>
  terra::as.data.frame(xy = TRUE)
xyz |> slice(1:5)


## ------------------
ggplot(xyz) +
  geom_raster(aes(x = x, y = y, fill = hillshade)) +
  scale_fill_continuous_c4a_seq(palette = "brewer.greys") +
  coord_sf() +
  guides(fill = "none")


## ------------------
ggplot() +
  geom_sf(data = ak_areas, aes(fill = nz_european)) +
  scale_fill_continuous_c4a_seq(palette = "brewer.reds") +
  geom_raster(data = xyz, aes(x = x, y = y, fill = hillshade), alpha = 0.5) +
  scale_fill_continuous_c4a_seq(palette = "brewer.greys") +
  guides(fill = "none")


## ------------------
library(ggnewscale)

ggplot() +
  geom_sf(data = ak_areas, aes(fill = nz_european)) +
  scale_fill_continuous_c4a_seq(palette = "brewer.reds") +
  guides(fill = "none") +
  new_scale_fill() +
  geom_raster(data = xyz, aes(x = x, y = y, fill = hillshade), alpha = 0.5) +
  scale_fill_continuous_c4a_seq(palette = "brewer.greys") +
  guides(fill = "none")


## ------------------
library(tmap)
tmap_mode("plot")  #<1>


## ------------------
tm_shape(ak_areas) +
  tm_polygons(
    "nz_european",
    fill.scale = tm_scale_intervals( 
      style = "equal", n = 5,
      values = "brewer.reds")
    )


## ------------------
tm_shape(ak_areas) +
  tm_polygons("nz_european",
              fill.scale = tm_scale_intervals(values = "brewer.reds"))


## ------------------
qtm(ak_areas, fill = "nz_european")


## ------------------
tmap_options(
  legend.frame = FALSE,
  frame = FALSE
)


## ------------------
map1 <- tm_shape(ak_areas) + 
  tm_polygons("pacific", 
    fill.scale = tm_scale_intervals("equal", n = 7), lwd = 0.2) +
  tm_title("Equal interval", size = 1)
map2 <- tm_shape(ak_areas) + 
  tm_polygons("pacific", 
    fill.scale = tm_scale_intervals("quantile", n = 7), lwd = 0.2) +
  tm_title("Quantiles", size = 1)
map3 <- tm_shape(ak_areas) + 
  tm_polygons("pacific", 
    fill.scale = tm_scale_intervals("sd", n = 7), lwd = 0.2) +
  tm_title("Std. deviation", size = 1)
map4 <- tm_shape(ak_areas) + 
  tm_polygons("pacific", 
    fill.scale = tm_scale_intervals("fisher", n = 7), lwd = 0.2) + 
  tm_title("Fisher method", size = 1)
tmap_arrange(map1, map2, map3, map4, ncol = 2, nrow = 2)


## ------------------
tm_shape(ak_areas) +
  tm_polygons(
    "asian", col = "white", lwd = 0.35,
    fill.scale = tm_scale_intervals(breaks = c(0, 1, 2, 5, 10, 15, 20, 30, 60)))


## ------------------
tm_shape(ak_areas) +
  tm_fill("nz_european",
    fill.scale = tm_scale_continuous(values = "-hcl.reds3"),
    fill_alpha = 0.75)+
  tm_fill("asian",
    fill.scale = tm_scale_continuous(values = "-hcl.blues3"),
    fill_alpha = 0.75) +
  tm_borders(col = "black")


## ------------------
tm_shape(ak_context, bbox = st_bbox(ak_areas)) +
  tm_fill() +
  tm_shape(ak_areas) +
  tm_polygons(fill = "grey", col = "white")


## ------------------
tm_shape(ak_areas) +
  tm_polygons() +
  tm_compass() +
  tm_scalebar(position = c("left", "bottom"))


## ------------------
elevation <- rast("data/ak-dem.tif")
tmap_options(raster.max_cells = 50000)
tm_shape(ak_areas) +
  tm_polygons("maori") +
  tm_shape(hillshade) +
  tm_raster(col.scale = tm_scale_continuous(values = "brewer.greys"), 
            col_alpha = 0.5) +
  tm_layout(legend.show = FALSE)


## ------------------
elevation <- rast("data/ak-dem.tif")
tm_shape(elevation) +
  tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain")) +
  tm_shape(hillshade) +
  tm_raster(col.scale = tm_scale_continuous(values = "brewer.greys"), 
            col_alpha = 0.5) +
  tm_layout(legend.show = FALSE)


## ------------------
m <- tm_shape(ak_areas) + 
  tm_polygons(
    fill = c("asian", "maori", "nz_european", "pacific"),
    fill.free = FALSE,
    fill.legend = tm_legend("% population")) +
  tm_layout(panel.label.bg.color = NA,
            panel.label.frame = FALSE) +
  tm_facets(nrow = 2, ncol = 2)


## ------------------
m


## ------------------
tm_shape(ak_areas) +
  tm_polygons(col = "white") +
  tm_text("suburb", options = opt_tm_text(remove_overlap = TRUE), size = 0.7)


## ------------------
tmap_mode("view")
tm_shape(ak_areas) +
  tm_polygons("tb_per_100k", hover = "suburb")

