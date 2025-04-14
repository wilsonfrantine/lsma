#' @export
plot.lsma <- function(x, site = NULL, ...) {
  if (!inherits(x, "lsma")) stop("'x' must be an object of class 'lsma'")
  
  available_sites <- names(x$rasters)
  
  if (is.null(site)) {
    for (site_name in available_sites) {
      terra::plot(
        x$rasters[[site_name]],
        main = paste("Site:", site_name),
        nc = length(x$buffers),
        ...
      )
    }
  } else {
    if (!site %in% available_sites) {
      stop("Site '", site, "' not found in lsma object. Available sites: ",
           paste(available_sites, collapse = ", "))
    }
    
    rast <- x$rasters[[site]]
    buf_names <- paste0("buf", x$buffers)
    names(rast) <- buf_names
    
    terra::plot(rast, main = paste("Site:", site), nc = length(x$buffers), ...)
  }
}

