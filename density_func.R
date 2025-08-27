#' Calculate building density per tile using a Gaussian kernel
#'
#' This function computes the density of buildings within a specific tile,
#' applying a Gaussian kernel to smooth the values and producing a raster
#' that matches the resolution of the provided template raster. It is ideal
#' for processing large areas by tiles without loading all data into memory.
#'
#' @param tile_geom An sf object representing a polygon tile where density
#'   will be calculated.
#' @param buildings An sf object containing building centroids.
#' @param r_template A terra raster defining the resolution and extent of the output.
#' @param sigma Numeric. Standard deviation of the Gaussian kernel in meters.
#'   Controls the amount of smoothing. Default is 333.
#' @param buffer_factor Numeric. Multiplier of sigma to define the buffer
#'   around the tile. Ensures kernel smoothing near tile edges. Default is 2.
#' @param unit_scale Numeric. Factor to convert the output density units.
#'   For example, 1e6 converts density to buildings per km². Default is 1e6.
#'
#' @return A terra raster of building density for the given tile. Returns NULL
#'   if no buildings are present within the buffered tile.
#'
#' @import terra sf
#'
#' @details
#' The function works in the following steps:
#' 1. Creates a buffer around the tile proportional to `sigma * buffer_factor`.
#' 2. Filters building centroids within the buffered tile.
#' 3. Crops the raster template to the buffered tile extent.
#' 4. Rasterizes building points to the template resolution.
#' 5. Constructs a normalized Gaussian kernel.
#' 6. Applies a focal (convolution) operation to compute smoothed density.
#' 7. Converts density values to the specified units using `unit_scale`.
#' 8. Crops the resulting raster to the original tile extent.
#'
#' @examples
#' \dontrun{
#' # Preload your data
#' tiles <- st_read("tiles.gpkg")
#' buildings <- st_read("centroides_edificaciones.gpkg")
#' r_template <- rast("raster_template.tif")
#'
#' # Process a single tile
#' density_tile <- process_tile_density(
#'   tile_geom = tiles[1,],
#'   buildings = buildings,
#'   r_template = r_template,
#'   sigma = 333,
#'   buffer_factor = 2,
#'   unit_scale = 1e6
#' )
#' }
process_tile_density_gaussian <- function(tile_geom,
                                 buildings,
                                 r_template,
                                 sigma = 333,
                                 buffer_factor = 2,
                                 unit_scale = 1e6) {
require(terra)
require(sf)

# 1. Create buffer around tile
buffer_dist <- sigma * buffer_factor
tile_buffer <- st_buffer(tile_geom, buffer_dist)

# 2. Filter buildings within buffered tile

bbox_aoi <- st_bbox(tile_buffer)
coords <- st_coordinates(buildings)

# Filtrado por coordenadas (sin geometría todavía)
mask <- coords[,1] >= bbox_aoi["xmin"] &
  coords[,1] <= bbox_aoi["xmax"] &
  coords[,2] >= bbox_aoi["ymin"] &
  coords[,2] <= bbox_aoi["ymax"]

buildings_tile <- buildings[mask, ]
if (nrow(buildings_tile) == 0) return(NULL)
buildings_tile <- buildings_tile %>% mutate(c = 1)

# 3. Crop raster template to buffered tile
r_tile <- crop(r_template, vect(tile_buffer))

# 4. Rasterize building points
build_rast <- rasterize(vect(buildings_tile), r_tile, field = "c", fun = "sum", background = 0)

# 5. Create Gaussian kernel
res_m <- res(r_tile)[1]
radius <- ceiling(buffer_factor * sigma / res_m)
x <- seq(-radius, radius)
gauss_kernel <- exp(-(x^2) / (2 * (sigma / res_m)^2))
kernel <- outer(gauss_kernel, gauss_kernel)
kernel <- kernel / sum(kernel)

# 6. Apply focal operation to compute density
density_raster <- focal(build_rast, w = kernel, fun = sum, na.policy = "omit", pad = TRUE)

# 7. Convert to desired units
density_raster <- density_raster * (unit_scale / (res_m^2))

# 8. Crop to original tile
density_raster <- crop(density_raster, vect(tile_geom)) %>% mask(.,vect(tile_geom))
plot(density_raster)
return(density_raster)
}

#' Calculate building density per tile using a uniform (non-smoothed) kernel
#'
#' This function computes the density of buildings within a specific tile,
#' applying a uniform kernel (box filter) instead of a Gaussian kernel.
#' This produces a local sum of buildings over a specified radius without
#' smoothing the weights.
#'
#' @param tile_geom An sf object representing a polygon tile where density
#'   will be calculated.
#' @param buildings An sf object containing building centroids.
#' @param r_template A terra raster defining the resolution and extent of the output.
#' @param sigma Numeric. Radius distance in meters for the uniform kernel.
#'   This is interpreted as half the window size. Default is 333.
#' @param buffer_factor Numeric. Multiplier of sigma to define the buffer
#'   around the tile. Ensures kernel calculation near tile edges. Default is 2.
#' @param unit_scale Numeric. Factor to convert the output density units.
#'   For example, 1e6 converts density to buildings per km². Default is 1e6.
#'
#' @return A terra raster of building density for the given tile. Returns NULL
#'   if no buildings are present within the buffered tile.
#'
#' @details
#' The function replaces the Gaussian kernel with a uniform kernel, meaning
#' all cells within the window contribute equally. This is useful when you
#' want an exact local average/count without weighting by distance.
#'
#' @examples
#' \dontrun{
#' tiles <- st_read("tiles.gpkg")
#' buildings <- st_read("centroides_edificaciones.gpkg")
#' r_template <- rast("raster_template.tif")
#'
#' density_tile <- process_tile_density_uniform(
#'   tile_geom = tiles[1,],
#'   buildings = buildings,
#'   r_template = r_template,
#'   sigma = 333,
#'   buffer_factor = 2,
#'   unit_scale = 1e6
#' )
#' }
process_tile_density_uniform <- function(tile_geom,
                                         buildings,
                                         r_template,
                                         buffer_dist = 2000,
                                         search_radius=100,
                                         unit_scale = 1e6) {
  require(terra)
  require(sf)
  
  # 1. Create buffer around tile
  tile_buffer <- st_buffer(tile_geom, buffer_dist)
  
  # 2. Filter buildings within buffered tile (fast bbox filter)
  bbox_aoi <- st_bbox(tile_buffer)
  coords <- st_coordinates(buildings)
  mask <- coords[,1] >= bbox_aoi["xmin"] &
    coords[,1] <= bbox_aoi["xmax"] &
    coords[,2] >= bbox_aoi["ymin"] &
    coords[,2] <= bbox_aoi["ymax"]
  buildings_tile <- buildings[mask, ]
  if (nrow(buildings_tile) == 0) return(NULL)
  buildings_tile <- buildings_tile %>% mutate(c = 1)
  
  # 3. Crop raster template to buffered tile
  r_tile <- crop(r_template, vect(tile_buffer))
  
  # 4. Rasterize building points
  build_rast <- rasterize(vect(buildings_tile), r_tile, field = "c", fun = "sum", background = 0)
      
  # 5. Convert to density (buildings per unit area, e.g., km²)
  cell_area <- res(r_tile)[1]^2
  radius <- ceiling(search_radius/res(r_tile)[1])
  kernel <- matrix(1, nrow = (2 * radius + 1), ncol = (2 * radius + 1))
  density_sum   <- focal(build_rast, w = kernel, fun = sum, na.policy = "omit", pad = TRUE)
  window_area   <- sum(kernel) * cell_area
  density_raster  <- density_sum / window_area * 1e6

  
  # 6. Crop to original tile
  density_raster <- crop(density_raster, vect(tile_geom)) %>% mask(.,vect(tile_geom))
  
  
  plot(density_raster)
  return(density_raster)
}
