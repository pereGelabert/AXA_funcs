# Tile creation
## Usage:

```r
source("https://raw.githubusercontent.com/pereGelabert/AXA_funcs/refs/heads/main/tiling_chile.R")

# 1. Load main Chile polygon and filter large polygons (mainland)
Chile <- st_read("./Chile_main.gpkg") %>%
  dplyr::select(NAME)

plot(main_Chile)

# 2. Split the polygon into ~200 tiles
pol_areas <- split_poly(main_Chile, 200) %>% 
  dplyr::select(id, area) 

plot(pol_areas)

# 3. Save tiles to GeoPackage
st_write(pol_areas, "./tiles.gpkg", append = FALSE)
```

# Get forest patches

## Usage
```r

source("https://raw.githubusercontent.com/pereGelabert/AXA_funcs/refs/heads/main/get_forest_patches.R")

ForestPatches <- get_dist_Forestpatches(
  ESA_WC = "./ESA_WC/LC_IDtile-1_cl.tif",
  tiles = "./tiles.gpkg",
  tile_id = 1,
  buffer_dist = 5000,      # Buffer around the tile in meters
  buffer_patchDist = 2000  # Distance threshold to forest patches in meters)
```
---
# Get building density
## Usage:
```r
source("https://raw.githubusercontent.com/pereGelabert/AXA_funcs/refs/heads/main/density_func.R")
tiles <- st_read("tiles.gpkg")
buildings <- st_read("centroides_edificaciones.gpkg")
r_template <- rast("raster_template.tif")

output_rasters <- list()

for (i in seq_len(nrow(tiles))) {
  cat("Processing tile", i, "of", nrow(tiles), "\n")
  
  r_tile <- process_tile_density(
    tile_geom = tiles[i, ],
    buildings = buildings,
    r_template = r_template,
    sigma = 333,
    buffer_factor = 2,
    unit_scale = 1e6
  )
  
  if (!is.null(r_tile)) {
    output_rasters[[i]] <- r_tile
    writeRaster(r_tile, paste0("densidad_tile_", i, ".tif"), overwrite = TRUE)
  }
}

r_final <- do.call(mosaic, output_rasters)
writeRaster(r_final, "densidad_chile.tif", overwrite = TRUE)
```
