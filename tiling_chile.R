
#' Split a polygon into approximately equal-area sub-polygons
#'
#' This function takes a polygon (sf object) and divides it into `n_areas` sub-polygons 
#' using random points and Voronoi tessellation. The result is clipped to the original polygon.
#'
#' @param sf_poly An sf polygon object to be split.
#' @param n_areas Number of sub-polygons desired.
#' @return An sf object with `n_areas` sub-polygons, each with an `area` column.
#' @import sf tidyverse dismo units

tiling<- function(sf_poly, n_areas) {
  require(sf)
  require(tidyverse) 
  require(dismo)
  require(units)

  # 1. Generate random points within the polygon
  points_rnd <- st_sample(sf_poly, size = 10000)
  
  # 2. Extract coordinates and convert to tibble for k-means
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  
  # 3. Apply k-means clustering to points to determine cluster centers
  k_means <- kmeans(points, centers = n_areas)
  
  # 4. Create Voronoi polygons based on cluster centers
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  
  # 5. Set CRS to match the input polygon
  crs(voronoi_polys) <- crs(sf_poly)
  
  # 6. Convert to sf and clip polygons to original polygon
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly) %>% 
    group_by(id) %>% summarise()
  
  # 7. Compute area of each sub-polygon
  equal_areas$area <- st_area(equal_areas)
  
  return(equal_areas)
}
