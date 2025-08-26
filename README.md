# Get forest patches

## Usage
```r

source("https://raw.githubusercontent.com/pereGelabert/AXA_funcs/refs/heads/main/get_forest_patches.R)

ForestPatches <- get_dist_Forestpatches(
  ESA_WC = "./ESA_WC/LC_IDtile-1_cl.tif",
  tiles = "./tiles.gpkg",
  tile_id = 1,
  buffer_dist = 5000,      # Buffer around the tile in meters
  buffer_patchDist = 2000  # Distance threshold to forest patches in meters)
```
