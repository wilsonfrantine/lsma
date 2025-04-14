#' @name create_lsma
#' @title Create an `lsma` object
#' @description Construct a structured object of class `lsma`, used to store landscape rasters and metadata for multi-scale analysis. Used internally to structure landscape objects. Users typically do not need to call this function directly.
#' @param rasters A named list of `SpatRaster` objects (from the terra package), one per site.
#' @param points A `SpatVector`, `sf`, or `SpatialPoints*` object with sampling sites.
#' @param buffers A numeric vector of buffer radii used to generate the rasters.
#' @param strategy A character string: one of `"nested"`, `"decouple"`, or `"partial_decouple"`.
#' @param buffers_sf (optional) A named list of `SpatVector` objects representing the original buffer geometries used during extraction, one per site.
#' @param ... Reserved for future use.
#' @return An object of class `lsma`, containing metadata and raster data.
#' @export
create_lsma <- function(rasters, points, buffers, strategy, buffers_sf = NULL, ...) {
  if (!all(vapply(rasters, inherits, logical(1), what = "SpatRaster"))) {
    stop("All elements in 'rasters' must be 'SpatRaster' objects.")
  }
  
  ref_raster <- rasters[[1]]
  
  if (!inherits(ref_raster, "SpatRaster")) {
    stop("All rasters must be 'SpatRaster' objects from the terra package.")
  }
  
  raster_info <- list(
    projection = terra::crs(ref_raster, proj = TRUE),
    resolution = terra::res(ref_raster),
    extent = terra::ext(ref_raster),
    n_layers = terra::nlyr(ref_raster),
    n_sites = length(rasters)
  )
  
  structure(
    list(
      rasters = rasters,
      points = points,
      buffers = buffers,
      strategy = strategy,
      raster_info = raster_info,
      buffers_sf = buffers_sf
    ),
    class = "lsma"
  )
}
