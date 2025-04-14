#' @title Summary for lsma objects
#' @name summary.lsma
#' @description Display a summary of an object of class `lsma`, including metadata and structural information.
#' @param object An object of class `lsma`.
#' @param ... Additional arguments (currently ignored).
#' @export
summary.lsma <- function(object, ...) {
  cat("lsma object summary\n")
  cat("-------------------\n")
  cat("Number of sites:       ", object$raster_info$n_sites, "\n")
  cat("Layers per site:       ", object$raster_info$n_layers, "\n")
  cat("Projection:            ", object$raster_info$projection, "\n")
  cat("Resolution:            ", paste(object$raster_info$resolution, collapse = " x "), "\n")
  cat("Buffer sizes (meters): ", paste(object$buffers, collapse = ", "), "\n")
  cat("Strategy:              ", object$strategy, "\n")
}
