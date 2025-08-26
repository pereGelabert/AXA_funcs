#' Calculate distance to large forest patches within a buffer
#'
#' This function computes a raster representing areas within a given distance
#' from large forest patches (≥ 5 km²) based on ESA WorldCover data. It crops the input raster
#' using a buffered tile geometry, identifies forest patches, calculates distance,
#' and returns a binary raster where 1 indicates proximity to forest patches.
#'
#' @param ESA_WC Character. Path to the ESA WorldCover raster file.
#' @param tiles Character. Path to a vector file (GeoPackage or Shapefile) containing tile geometries.
#' @param tile_id Integer or character. ID of the tile to process (must match the `id` column in `tiles`).
#' @param buffer_dist Numeric. Buffer distance (in meters) to apply around the tile when cropping the ESA raster.
#' Default is 5000.
#' @param buffer_patchDist Numeric. Maximum distance (in meters) from forest patches to classify as 1.
#' Default is 2000.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Loads the tile geometry for the specified `tile_id`.
#'   \item Crops ESA WorldCover raster with a buffer around the tile.
#'   \item Identifies forest classes (10 = Tree Cover, 20 = Shrubland).
#'   \item Converts forest raster to polygons, filters patches larger than 5 km².
#'   \item Rasterizes the forest patches and calculates distance to them.
#'   \item Creates a binary raster where 1 indicates areas within `buffer_patchDist` of a forest patch.
#'   \item Crops the result back to the original tile geometry.
#' }
#'
#' @return A `SpatRaster` object (from the `terra` package) representing the proximity to forest patches.
#' The raster contains `1` where the distance to a forest patch is less than `buffer_patchDist`, and `NA` elsewhere.
#'
#' @examples
#' \dontrun{
#' result <- get_dist_Forestpatches(
#'   ESA_WC = "./ESA_WC/LC_IDtile-1_cl.tif",
#'   tiles = "./tiles.gpkg",
#'   tile_id = 1,
#'   buffer_dist = 5000,
#'   buffer_patchDist = 2000
#' )
#' plot(result)
#' }
#'
#' @import terra sf stars units dplyr
#' @export
get_dist_Forestpatches <- function(ESA_WC, tiles, tile_id, buffer_dist = 5000, buffer_patchDist = 2000){
  require(terra)
  require(sf)
  require(stars)
  require(units)
  require(dplyr)
  
  # 1. Load the tile geometry for the specified ID
  tile <- st_read(tiles, quiet = TRUE) %>% 
    filter(id == tile_id)
  
  if(nrow(tile) == 0){
    stop(paste("Tile with id", tile_id, "was not found"))
  }
  
  # 2. Load ESA LandCover raster and crop using the tile with buffer
  landcover <- rast(ESA_WC)
  tile_buffer <- st_buffer(tile, buffer_dist)
  
  landcover_crop <- crop(landcover, vect(tile_buffer))
  
  # 3. Select only forest classes (10 = Tree Cover, 20 = Shrubland)
  forest <- ifel(landcover_crop %in% c(10, 20), 1, NA)
  
  # 4. Convert to sf polygons and filter patches > 5 km²
  forest_patches <- st_as_stars(forest) %>% 
    st_as_sf(merge = TRUE) %>%
    mutate(area = st_area(.)) %>%
    filter(area >= set_units(5, "km^2")) %>%
    mutate(id = row_number()) %>%
    rename(forest = 1)
  
  if(nrow(forest_patches) == 0){
    warning(paste("No forest patches > 5 km² found in tile", tile_id))
    return(NULL)
  }
  
  # 5. Rasterize the forest patches for distance calculation
  forest_raster <- rasterize(vect(forest_patches), landcover_crop, field = "forest")
  
  # 6. Compute distance from forest patches
  d <- distance(forest_raster)
  
  # 7. Classify areas within buffer_patchDist as 1, others as NA
  d_masked <- ifel(d < buffer_patchDist, 1, NA)
  
  # 8. Crop and mask to the original tile geometry
  d_tile <- crop(d_masked, vect(tile)) %>% mask(vect(tile))
  
  # 9. Quick visualization
  plot(d_tile, main = paste("Distance to forest patches (tile", tile_id, ")"))
  plot(st_geometry(tile), add = TRUE, border = "red", lwd = 2)
  
  return(d_tile)
}
