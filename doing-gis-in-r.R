## -------------------
# Doing GIS in R - workshop © 2025 
# by David O'Sullivan is licensed under 
# CC-BY-NC-SA 4.0
# https://creativecommons.org/licenses/by-nc-sa/4.0/


## -------------------
# install.packages(
#   c("tidyverse",
#     "dplyr",
#     "sf",
#     "tmap",
#     "terra")
# )


## -------------------
library(tidyverse)
library(dplyr)
library(sf)
library(tmap)
nz <- st_read("data/nz.gpkg")


## -------------------
qtm(World, "pop_est_dens")


## -------------------
World |> head()


## -------------------
pt <- st_point(c(100, 100))
mpt <- st_multipoint(matrix(runif(100, min = 50, max = 150), ncol = 2))


## -------------------
quakes <- read_csv("data/earthquakes.csv")
head(quakes) |> as_tibble()


## -------------------
quakes_geo <- quakes |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = FALSE) |>
  st_set_crs(4326)


## -------------------
qtm(nz) +
qtm(quakes_geo, 
    size = 0.2, 
    fill = "red", 
    lwd = 0)


## -------------------
nz <- st_read("data/nz.gpkg")


## -------------------
qtm(World) + tm_graticules()


## -------------------
World |>
  st_transform("+proj=moll") |>
  qtm() + tm_graticules(x = -6:6 * 30, y = -3:3 * 30)


## -------------------
World |> st_transform("+proj=ortho +lon_0=180") |>
  qtm() + tm_graticules(c(-90,120,150,180,-150,-120,-90), -3:3 * 30)


## -------------------
World |> st_transform("ESRI:53077") |>
  qtm()


## -------------------
nz |> st_crs()


## -------------------
map1 <- qtm(World, bbox = st_bbox(nz), fill = "#0000ff40") +
  qtm(nz, fill = "#ff000040", col = "red") + tm_grid()
map2 <- qtm(nz, fill = "#ff000040", col = "red") +
  qtm(World, fill = "#0000ff40") + tm_grid()
tmap_arrange(map1, map2)


## -------------------
nz2 <- st_read("data/nz2.gpkg")


## -------------------
plot(nz2$geom)


## -------------------
kindergartens <- st_read("data/kindergartens.gpkg")
kindergartens_1000 <- kindergartens |> st_buffer(1000)


## -------------------
qtm(nz, lwd = 0, 
    bbox = kindergartens_1000) +
qtm(kindergartens_1000, 
    fill = "dodgerblue", fill.alpha = 0.3, 
    lwd = 0) +
qtm(kindergartens, 
    size = 0.2)


## -------------------
welly_sa2 <- st_read("data/wellington-city-sa2.gpkg")
welly_sa2_c <- welly_sa2 |>
  st_centroid()


## -------------------
qtm(nz, lwd = 0, 
    bbox = welly_sa2) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(welly_sa2_c, 
    size = 0.2)


## -------------------
kelburn <- welly_sa2 |>
  slice(36)
kelburn_hull <- kelburn |>
  st_convex_hull()
qtm(kelburn) +
qtm(kelburn_hull, 
    fill = "#ff000040")


## -------------------
circles <- welly_sa2 |> 
  st_minimum_bounding_circle()
rectangles <- welly_sa2 |> 
  st_minimum_rotated_rectangle()


## -------------------
qtm(nz, lwd = 0,
    bbox = welly_sa2) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(circles, 
    fill = "#ff000030", 
    col = "red")


## -------------------
qtm(nz, lwd = 0,
    bbox = welly_sa2) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(rectangles, 
    fill = "#0000ff30", 
    col = "blue")


## -------------------
welly_sa2


## -------------------
welly_data <- read.csv("data/wellington-region-sa2-data.csv")
welly_data


## -------------------
welly_gdf <- welly_sa2 |>
  left_join(welly_data)
welly_gdf


## -------------------
qtm(nz2,  lwd = 0, 
    bbox = welly_gdf) + 
qtm(welly_gdf, 
    fill = "pink", 
    lwd = 0.35) + 
qtm(kindergartens, 
    size = 0.4, 
    fill = "red")


## -------------------
kindergartens_data <- kindergartens |>
  st_join(welly_gdf)
kindergartens_data |> as_tibble()


## -------------------
kindergartens |> st_within(welly_gdf, sparse = TRUE)


## -------------------
school_zones <- st_read("data/school-zones.gpkg")


## -------------------
qtm(nz, lwd = 0,
    bbox = welly_gdf) + 
qtm(welly_gdf, lwd = 0.2,
    fill = "pink") + 
qtm(school_zones, lwd = 0.3, 
    fill = "dodgerblue", fill.alpha = 0.3) + 
qtm(kindergartens, 
    size = 0.35)


## -------------------
school_zones |> st_join(welly_gdf)


## -------------------
school_zones |> st_join(welly_gdf) |>
  group_by(School_name) |>
  summarise(pop     = sum(pop),
            age_0_4 = sum(age_0_4))


## -------------------
school_zones |> st_join(welly_gdf, largest = TRUE)


## -------------------
n <- school_zones |>
  st_contains(kindergartens) |>
  lengths()
school_zones$n_kindies <- n
school_zones


## -------------------
welly_gdf |>
  st_filter(kindergartens) |>
  qtm()


## -------------------
school_zones_in_kelburn <- school_zones |>
  st_intersection(kelburn |> st_buffer(1000))
qtm(welly_gdf, bbox = kelburn |> st_buffer(1500)) +
  qtm(kelburn, fill = "darkgrey", lwd = 0) +
  qtm(school_zones_in_kelburn, fill = "#ff000040", col = "white")


## -------------------
k_bbox <- kindergartens |>
  st_buffer(2000) |>
  st_bbox() |>
  st_as_sfc() |>
  st_sf()
k_voronoi <- kindergartens |>
  st_union() |> 
  st_voronoi() |> 
  st_cast() |>
  st_as_sf() |> 
  st_intersection(nz) |>
  st_intersection(k_bbox)


## -------------------
qtm(nz, lwd = 0,
    bbox = kindergartens) +
qtm(k_voronoi, 
    fill = "red", fill_alpha = 0.3, 
    col = "white") +
qtm(kindergartens, 
    size = 0.2)


## -------------------
library(terra)


## -------------------
dem <- rast("data/ak-dem.tif")
dem


## -------------------
tm_shape(dem) + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
layers <- dir("data/raster-stack", pattern = ".*.tif", 
              full.names = TRUE)
welly_stack <- rast(layers)
welly_stack


## -------------------
tm_shape(welly_stack) + tm_raster()


## -------------------
library(geodata)
elev <- rast("data/elevation/srtm_71_21.tif")
elev


## -------------------
qtm(elev) + qtm(welly_stack$mat)


## -------------------
welly_elev <- elev |> 
  terra::project(welly_stack$mat)
tm_shape(welly_elev) + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
# elev |> project(welly_stack$mat, align_only = TRUE)


## -------------------
# elev |> project("EPSG:2193")
# elev |> project(st_crs(welly_gdf)$wkt)


## -------------------
# elev |> project("EPSG:2193", res = 100)


## -------------------
welly_elev |> 
  terra::aggregate(5) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
welly_elev |> 
  disagg(5, method = "bilinear") |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
tmap_arrange(list(
    tm_shape(welly_elev) + 
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2")),
    tm_shape(welly_elev |> terra::aggregate(5) |> disagg(5, method = "bilinear")) + 
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))))


## -------------------
warm <- welly_stack$mat > 120.4
dry  <- welly_stack$rain > 1159.7
warm_and_dry <- (warm & dry) |> mask(warm)
names(warm_and_dry) <- c("WarmDry")
tmap_arrange(list(qtm(warm), qtm(dry), qtm(warm_and_dry)))


## -------------------
welly_elev |>
  focal(w = 7, fun = "sd", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
weights <- matrix(c( 0, -1, 0,
                    -1,  0, 1,
                     0,  1, 0), ncol = 3)
welly_elev |>
  focal(w = weights, fun = "sum", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
w <- focalMat(welly_elev, 200, "Gauss")
welly_elev |>
  focal(w, fun = "sd", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## -------------------
slope <- terra::terrain(welly_elev, unit = "radians")
aspect <- terra::terrain(welly_elev, v = "aspect", unit = "radians")
hillshade <- terra::shade(slope, aspect, angle = 135)


## -------------------
  tm_shape(hillshade) + 
  tm_raster(col.scale = tm_scale_continuous(values = "brewer.greys"),
            col_alpha = 0.5)


## -------------------
welly_elev |> terra::extract(
  kindergartens |> as("SpatVector"), bind = TRUE, ID = FALSE) |>
  st_as_sf()


## -------------------
welly_elev |> terra::extract(
  school_zones |> as("SpatVector"), fun = "mean", bind = TRUE, ID = FALSE) |>
  st_as_sf()
