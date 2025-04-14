#' @name extract_landscapes
#' @title Extract multi-scale landscapes
#' @description Cut a raster around sampling points using different buffer radii and strategies. Returns a structured object of class `lsma`.
#' @param r A `Raster*` or `SpatRaster` object.
#' @param p A set of sampling points (`sf`, `SpatVector`, or `SpatialPoints*`).
#' @param buffers A numeric vector of buffer sizes (in map units).
#' @param strategy The extraction strategy: `"nested"`, `"decouple"` or `"partial_decouple"`.
#' @param ... Reserved for future use.
#' @return An object of class `lsma`.
#' @export

extract_landscapes <- function(r, p, buffers, strategy = "nested", keep_buffer=NULL) {
  if (inherits(r, "Raster")) {
    r <- terra::rast(r)
  }
  if (!inherits(r, "SpatRaster")) {
    stop("'r' must be a Raster* or SpatRaster object.")
  }
  
  if (inherits(p, "sf")) {
    p <- terra::vect(p)
  } else if (inherits(p, "Spatial")) {
    p <- terra::vect(sf::st_as_sf(p))
  } else if (!inherits(p, "SpatVector")) {
    stop("'p' must be an sf, SpatVector, or Spatial* object.")
  }
  
  if (!is.numeric(buffers) || length(buffers) == 0) {
    stop("'buffers' must be a numeric vector of buffer sizes.")
  }
  
  strategy <- match.arg(strategy, choices = c("nested", "decouple", "partial_decouple"))
  
  strategy_result <- switch(strategy,
                            "nested" = nested_buffers(r, p, buffers),
                            "decouple" = decouple_buffers(r, p, buffers),
                            "partial_decouple" = partial_decouple_buffers(r, p, buffers, keep_buffer)
  )
  
  create_lsma(
    rasters = strategy_result$rasters,
    points = p,
    buffers = buffers,
    strategy = strategy,
    buffers_sf = strategy_result$buffers
  )
}
