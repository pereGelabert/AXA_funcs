#' Generate Wildland-Urban Interface (WUI) Raster for a Tile
#'
#' This function calculates WUI classes for a given tile based on land cover,
#' canopy height, canopy cover, and building density rasters. It uses forest
#' patch distance to distinguish Intermix and Interface WUI types.
#'
#' @param tiles `sf` object. Polygon geometries for tiles, must contain an `id` column.
#' @param tile_id Integer. ID of the tile to process (row index in `tiles`).
#' @param landcover_raster `SpatRaster` (terra). Land cover raster (e.g., ESA WorldCover).
#' @param canopy_height_raster `SpatRaster` (terra). Raster of tree height in meters.
#' @param canopy_cover_raster `SpatRaster` (terra). Raster of tree canopy cover (%) values.
#' @param buildings_raster `SpatRaster` (terra). Raster of building density (buildings/ha).
#' @param tree_height_min Numeric. Minimum tree height (meters) to be considered. Default is 4.
#' @param tree_cover_th Numeric. Minimum tree canopy cover (%) to classify as forest. Default is 50.
#' @param out.dir Character. Directory path to save the output WUI raster. Required.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Crop all input rasters to the extent of the specified tile.
#'   \item Calculate forest patch distances using `get_dist_Forestpatches()` with default buffers.
#'   \item Initialize an empty WUI raster with `NA` values.
#'   \item Classify pixels as Intermix WUI (value = 1) where:
#'         - Tree canopy cover ≥ `tree_cover_th`
#'         - Building density ≥ 6.18 buildings/ha
#'         - Tree height ≥ `tree_height_min`
#'   \item Classify pixels as Interface WUI (value = 2) where:
#'         - Tree canopy cover < `tree_cover_th`
#'         - Building density ≥ 6.18 buildings/ha
#'         - Within forest buffer
#'         - Tree height ≥ `tree_height_min`
#'   \item Save the WUI raster to `out.dir` as a factor raster.
#' }
#'
#' @return A `SpatRaster` (terra) object of WUI classes masked to the tile geometry.
#'         - 1 = Intermix WUI  
#'         - 2 = Interface WUI  
#'         - NA = non-WUI
#'
#' @examples
#' \dontrun{
#' wui_tile1 <- generate_wui(
#'   tiles = tiles_sf,
#'   tile_id = 1,
#'   landcover_raster = LC_rast,
#'   canopy_height_raster = TH_rast,
#'   canopy_cover_raster = TC_rast,
#'   buildings_raster = buildings_rast,
#'   tree_height_min = 4,
#'   tree_cover_th = 50,
#'   out.dir = "./folder/"
#' )
#' plot(wui_tile1)
#' }
#'
#' @import terra sf
#' @export
generate_wui <- function(tiles, tile_id, 
                         landcover_raster, 
                         canopy_height_raster,
                         canopy_cover_raster,
                         buildings_raster,
                         tree_height_min = 4,
                         tree_cover_th = 50, 
                         out.dir = NULL){

  # 1. Extract the tile polygon
  tile <- tiles[tile_id, ]
  
  # 2. Crop all input rasters to the tile extent
  r_template <- crop(landcover_raster, tile)
  landcover_i <- crop(landcover_raster, tile)
  house_dens_i <- crop(buildings_raster, tile)
  TH_i <- crop(canopy_height_raster, tile)
  TC_i <- crop(canopy_cover_raster, tile)
  
  # 3. Compute forest patch distances using buffer
  forest_patches <- get_dist_Forestpatches(
    ESA_WC = landcover_raster,
    tiles = tiles,
    tile_id = tile_id,
    buffer_dist = 5000,
    buffer_patchDist = 2000
  )
  
  # 4. Initialize WUI raster with NA
  wui <- r_template
  wui[] <- NA
  
  # 5. Intermix WUI: high building density and trees above height threshold
  wui[TC_i >= tree_cover_th & house_dens_i >= 6.18 & TH_i >= tree_height_min] <- 1
  
  # 6. Interface WUI: outside forest but within buffer, trees above height threshold
  wui[TC_i < tree_cover_th & house_dens_i >= 6.18 & !is.na(forest_patches) & TH_i >= tree_height_min] <- 2
  
  # 7. Save WUI raster
  names(wui) <- "WUI"
  if(!is.null(out.dir)){
    out_file <- file.path(out.dir, paste0("WUI_30m_tile", tile_id, ".tif"))
    writeRaster(wui %>% as.factor(), out_file, overwrite = TRUE)
  }
  
  # 8. Return factor raster masked to tile geometry
  return(wui %>% as.factor() %>% mask(., vect(tile)))
}
