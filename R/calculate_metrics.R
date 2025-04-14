#' @title Calculate landscape metrics
#' @description Applies selected metrics to each raster layer in the `lsma` object using `landscapemetrics`.
#' @param x An object of class `lsma`.
#' @param level Character. One of `"patch"`, `"class"`, or `"landscape"`. Passed to `calculate_lsm()`.
#' @param metric Optional. Vector of metric names to calculate (e.g. `"pland"`, `"area"`). Default is all.
#' @param ... Additional arguments passed to `calculate_lsm`.
#' @return A tibble with calculated metrics by site and buffer.
#' @export
calculate_metrics <- function(x, level = "landscape", metric = NULL, ...) {
  if (!inherits(x, "lsma")) stop("'x' must be an object of class 'lsma'.")
  if (!requireNamespace("landscapemetrics", quietly = TRUE)) stop("Package 'landscapemetrics' is required.")
  if (!level %in% c("patch", "class", "landscape")) stop("Invalid level.")
  
  sites <- names(x$rasters)
  
  results <- purrr::map_dfr(sites, function(s) {
    rast <- x$rasters[[s]]
    layers <- terra::nlyr(rast)
    
    purrr::map_dfr(seq_len(layers), function(i) {
      r <- rast[[i]]
      lsm <- landscapemetrics::calculate_lsm(
        landscape = r,
        level = level,
        metric = metric,
        ...
      )
      lsm$site <- s
      lsm$buffer <- names(rast)[i]
      return(lsm)
    })
  })
  
  dplyr::as_tibble(results)
}
