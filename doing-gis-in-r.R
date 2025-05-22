## ------------------
# Doing GIS in R - workshop © 2025 
# by David O'Sullivan is licensed under 
# CC-BY-NC-SA 4.0
# https://creativecommons.org/licenses/by-nc-sa/4.0/


## ------------------
#| eval: false
# install.packages(c("sf", "terra"))


## ------------------
#| label: load-libraries
#| output: false
library(tidyverse)
library(dplyr)
library(sf)
library(tmap)

nz <- st_read("data/nz.gpkg")  # <1>


## ------------------
#| label: world-map
qtm(World, "pop_est_dens")  # <1>


## ------------------
#| label: world-sf-view
World |> head()


## ------------------
#| label: make-geometries
pt <- st_point(c(100, 100))
mpt <- st_multipoint(matrix(runif(100, min = 50, max = 150), ncol = 2))


## ------------------
#| label: read-quakes-data
quakes <- read_csv("data/earthquakes.csv")
head(quakes) |> as_tibble()


## ------------------
#| label: convert-quakes-to-sf
quakes_geo <- quakes |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = FALSE) |>  # <1>
  st_set_crs(4326)                                                  # <2>


## ------------------
#| label: quakes-map
#| classes: custom6535
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz) +
qtm(quakes_geo, 
    size = 0.2, 
    fill = "red", 
    lwd = 0)


## ------------------
#| label: read-nz-gpkg
nz <- st_read("data/nz.gpkg")


## ------------------
#| label: graticules-1
qtm(World) + tm_graticules()


## ------------------
#| label: graticules-mollweide
World |>
  st_transform("+proj=moll") |>
  qtm() + tm_graticules(x = -6:6 * 30, y = -3:3 * 30) # <1>


## ------------------
#| label: graticules-ortho
World |> st_transform("+proj=ortho +lon_0=180") |>
  qtm() + tm_graticules(c(-90,120,150,180,-150,-120,-90), -3:3 * 30)


## ------------------
#| label: transform-world-to-natural-earth
World |> st_transform("ESRI:53077") |>
  qtm()


## ------------------
#| label: show-wkt-data
nz |> st_crs()


## ------------------
#| label: tmap-automatic-projection
map1 <- qtm(World, bbox = st_bbox(nz), fill = "#0000ff40") +
  qtm(nz, fill = "#ff000040", col = "red") + tm_grid()
map2 <- qtm(nz, fill = "#ff000040", col = "red") +
  qtm(World, fill = "#0000ff40") + tm_grid()
tmap_arrange(map1, map2)


## ------------------
#| label: read-nz2.gpkg
nz2 <- st_read("data/nz2.gpkg")


## ------------------
#| label: map-nz2
plot(nz2$geom)


## ------------------
#| label: buffer-kindergartens-data
#| output: false
kindergartens <- st_read("data/kindergartens.gpkg")
kindergartens_1000 <- kindergartens |> st_buffer(1000)


## ------------------
#| label: map-buffers
#| classes: custom6535
#| output-location: column
#| fig-width: 3
#| fig-height: 6
qtm(nz, 
    lwd = 0, 
    bbox = kindergartens_1000) +
qtm(kindergartens_1000, 
    fill = "#0000ff30", 
    lwd = 0) +
qtm(kindergartens, 
    size = 0.2)


## ------------------
#| label: read-welly-sa2-data
#| output: false
welly_sa2 <- st_read("data/wellington-city-sa2.gpkg")
welly_sa2


## ------------------
#| label: st-centroid
welly_sa2_c <- welly_sa2 |>
  st_centroid()


## ------------------
#| label: map-centroids
#| classes: custom6535
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz, 
    lwd = 0, 
    bbox = welly_sa2) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(welly_sa2_c, 
    size = 0.2)


## ------------------
#| label: convex-hull
#| output-location: column
kelburn <- welly_sa2 |>
  slice(36)
kelburn_hull <- kelburn |>
  st_convex_hull()
qtm(kelburn) +
qtm(kelburn_hull, 
    fill = "#ff000040")


## ------------------
#| label: minimum-bounding-shapes
circles <- welly_sa2 |> 
  st_minimum_bounding_circle()

rectangles <- welly_sa2 |> 
  st_minimum_rotated_rectangle()


## ------------------
#| label: map-minimum-circles
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz, 
    bbox = welly_sa2, 
    lwd = 0) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(circles, 
    fill = "#ff000030", 
    col = "red")


## ------------------
#| label: map-minimum-rectangles
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz, 
    bbox = welly_sa2, 
    lwd = 0) +
qtm(welly_sa2, 
    fill = "grey", 
    col = "white") +
qtm(rectangles, 
    fill = "#0000ff30", 
    col = "blue")


## ------------------
#| label: show-welly-geom-data
welly_sa2


## ------------------
#| label: read-welly-census-data
welly_data <- read.csv("data/wellington-region-sa2-data.csv")
welly_data


## ------------------
#| label: table-join
welly_gdf <- welly_sa2 |>
  left_join(welly_data)
welly_gdf


## ------------------
#| label: map-welly-data-and-kindergartens
#| classes: custom6535
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz2, 
    fill = "lightgrey", 
    lwd = 0, 
    bbox = welly_gdf) + 
qtm(welly_gdf, 
    fill = "pink", 
    lwd = 0.35) + 
qtm(kindergartens, 
    size = 0.4, 
    fill = "red")


## ------------------
#| label: spatial-join-kindergartens-demography
kindergartens_data <- kindergartens |>
  st_join(welly_gdf)
kindergartens_data |> as_tibble()


## ------------------
#| label: kindergartens-within-polygons
kindergartens |> st_within(welly_gdf, sparse = TRUE)


## ------------------
#| label: read-school-zones-data
school_zones <- st_read("data/school-zones.gpkg")


## ------------------
#| label: map-school-zones-data
#| classes: custom6535
#| output-location: column
#| fig-width: 4
#| fig-height: 6
qtm(nz, 
    bbox = welly_gdf) + 
qtm(welly_gdf, 
    fill = "pink", 
    lwd = 0.1) + 
qtm(school_zones,
    lwd = 0.2, 
    col = "blue", 
    fill = "#0000ff30") + 
qtm(kindergartens, 
    size = 0.35)


## ------------------
#| label: polygon-polygon-join
school_zones |> st_join(welly_gdf)


## ------------------
#| label: group-by-summarise-spatial-join
school_zones |> st_join(welly_gdf) |>
  group_by(School_name) |>
  summarise(pop     = sum(pop),
            age_0_4 = sum(age_0_4))


## ------------------
#| label: largest-spatial-join
school_zones |> st_join(welly_gdf, largest = TRUE)


## ------------------
#| label: count-points-in-polygons
n <- school_zones |>
  st_contains(kindergartens) |>
  lengths()
school_zones$n_kindies <- n
school_zones


## ------------------
#| label: spatial-filter
#| classes: custom6535
#| output-location: column
#| fig-width: 4
#| fig-height: 6
welly_gdf |>
  st_filter(kindergartens) |>
  qtm()


## ------------------
#| label: clip-using-st-intersection
school_zones_in_kelburn <- school_zones |>
  st_intersection(kelburn |> st_buffer(1000))
qtm(welly_gdf, bbox = kelburn |> st_buffer(1500)) +
  qtm(kelburn, fill = "darkgrey", lwd = 0) +
  qtm(school_zones_in_kelburn, fill = "#ff000040", col = "white")


## ------------------
#| label: voronoi
#| output: false
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


## ------------------
#| label: voronoi-map
#| output-location: column
#| classes: custom6535
#| fig-width: 4
#| fig-height: 6
qtm(nz, 
    bbox = k_voronoi, 
    lwd = 0) +
qtm(k_voronoi, 
    fill = "#ff000040", 
    col = "white") +
qtm(kindergartens, 
    size = 0.2)


## ------------------
#| label: load-terra
#| output: false
library(terra)


## ------------------
#| label: read-a-raster
dem <- rast("data/ak-dem.tif")
dem


## ------------------
#| label: raster-map
tm_shape(dem) + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: read-raster-stack
layers <- dir("data/raster-stack", pattern = ".*.tif", full.names = TRUE)
welly_stack <- rast(layers)
welly_stack


## ------------------
#| label: map-raster-stack
tm_shape(welly_stack) + tm_raster()


## ------------------
#| label: download-elevation
library(geodata)
elev <- rast("data/elevation/srtm_71_21.tif")
elev


## ------------------
#| label: show-downloaded-data-and-wellington-data
qtm(elev) + qtm(welly_stack$mat)


## ------------------
#| label: project-raster
welly_elev <- elev |> 
  terra::project(welly_stack$mat)
tm_shape(welly_elev) + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| eval: false
# elev |> project(welly_stack$mat, align_only = TRUE)


## ------------------
#| eval: false
# elev |> project("EPSG:2193")
# elev |> project(st_crs(welly_gdf)$wkt)  # <1>


## ------------------
#| eval: false
# elev |> project("EPSG:2193", res = 100)


## ------------------
#| label: terra-aggregate
welly_elev |> 
  terra::aggregate(5) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: terra-disagg
welly_elev |> 
  disagg(5, method = "bilinear") |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: no-going-back
tmap_arrange(list(
    tm_shape(welly_elev) + 
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2")),
    tm_shape(welly_elev |> terra::aggregate(5) |> disagg(5, method = "bilinear")) + 
      tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))))


## ------------------
#| label: raster-calc
warm <- welly_stack$mat > 120.4
dry <- welly_stack$rain > 1159.7
warm_and_dry <- warm & dry |> mask(warm)
names(warm_and_dry) <- c("WarmDry")
tmap_arrange(list(qtm(warm), qtm(dry), qtm(warm_and_dry)))


## ------------------
#| label: focal-sd
welly_elev |>
  focal(w = 7, fun = "sd", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: focal-matrix
xyz <- expand_grid(x = 1:7, y = 1:7) |>
  mutate(w = 1 / (1 + sqrt((x - 4) ^ 2 + (y - 4) ^ 2)))
weights <- matrix(c( 0, -1, 0,
                    -1,  0, 1,
                     0,  1, 0), ncol = 3)
welly_elev |>
  focal(w = weights, fun = "sum", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: terra-focalMat
w <- focalMat(welly_elev, 200, "Gauss")
welly_elev |>
  focal(w, fun = "sd", na.rm = TRUE) |>
  mask(welly_elev) |>
  tm_shape() + tm_raster(col.scale = tm_scale_continuous(values = "hcl.terrain2"))


## ------------------
#| label: surface-analysis
slope <- terra::terrain(welly_elev, unit = "radians")
aspect <- terra::terrain(welly_elev, v = "aspect", unit = "radians")
hillshade <- terra::shade(slope, aspect, angle = 135)


## ------------------
#| label: map-hillshade
  tm_shape(hillshade) + 
  tm_raster(col.scale = tm_scale_continuous(values = "brewer.greys"),
            col_alpha = 0.5)


## ------------------
#| label: extract-to-points
welly_elev |> terra::extract(
  kindergartens |> as("SpatVector"), bind = TRUE, ID = FALSE) |>
  st_as_sf()


## ------------------
#| label: extract-to-polygons
welly_elev |> terra::extract(
  school_zones |> as("SpatVector"), fun = "mean", bind = TRUE, ID = FALSE) |>
  st_as_sf()

