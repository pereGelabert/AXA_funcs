# Get forest patches

## Usage
```r
result <- get_dist_Forestpatches(
  ESA_WC = "./ESA_WC/LC_IDtile-1_cl.tif",
  tiles = "./tiles.gpkg",
  tile_id = 1,
  buffer_dist = 5000,      # Buffer around the tile in meters
  buffer_patchDist = 2000  # Distance threshold to forest patches in meters
)
```
