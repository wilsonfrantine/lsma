#' @name plot_utils
#' @title Plot utils
#' @description TODO: Describe this function.
#' @export

modelplot <- function(models, ...) {

}


#' @title Interactive visualization of an lsma object
#' @description Visualize landscape rasters and/or buffer geometries using tmap. Supports single, multiple, or all sites.
#' @param x An object of class `lsma`.
#' @param site A site name (e.g. "p01"), a character vector of site names, or "all". If NULL, the first available site is shown.
#' @param what What to display: "both", "buffers", or "rasters".
#' @export
view_ls <- function(x, site = NULL, what = c("both", "buffers", "rasters")) {
  what <- match.arg(what)
  
  if (!inherits(x, "lsma")) stop("'x' must be an object of class 'lsma'.")
  if (!requireNamespace("tmap", quietly = TRUE)) stop("Package 'tmap' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("stars", quietly = TRUE)) stop("Package 'stars' is required.")
  
  tmap::tmap_mode("view")
  tmap::tmap_options(raster.max_cells = 1e6)
  
  sites <- names(x$rasters)
  
  if (is.null(site)) {
    site <- sites[1]
    message("No site specified. Showing first site: ", site)
  } else if (identical(site, "all")) {
    site <- sites
  } else if (!all(site %in% sites)) {
    stop("Invalid site name(s). Choose from: ", paste(sites, collapse = ", "), " or use 'all'.")
  }
  
  # Rasters
  rasters_list <- lapply(site, function(s) x$rasters[[s]][[terra::nlyr(x$rasters[[s]])]])
  mosaic_raster <- if (length(rasters_list) == 1) {
    rasters_list[[1]]
  } else {
    do.call(terra::mosaic, c(rasters_list, list(fun = "first")))
  }
  
  # Buffers
  buffers_list <- lapply(site, function(s) {
    sf_buf <- sf::st_as_sf(x$buffers_sf[[s]])
    sf_buf$site <- s
    sf_buf
  })
  merged_buffers <- do.call(rbind, buffers_list)
  
  # Nome dos sites: pontos originais
  points_sf <- sf::st_as_sf(x$points)
  points_sf$label <- names(x$rasters)
  points_sf <- points_sf[points_sf$label %in% site, ]
  
  # Raster para stars se necessário
  if (what %in% c("both", "rasters")) {
    tmpfile <- tempfile(fileext = ".tif")
    terra::writeRaster(mosaic_raster, filename = tmpfile, overwrite = TRUE)
    stars_layer <- stars::read_stars(tmpfile)
  }
  
  # Construção do mapa
  tm <- NULL
  if (what %in% c("both", "rasters")) {
    tm <- tmap::tm_shape(stars_layer) +
      tmap::tm_raster(col_alpha = 0.8)
  }
  if (what %in% c("both", "buffers")) {
    tm <- tm +
      tmap::tm_shape(merged_buffers) +
      tmap::tm_borders(col = "black") 
  }
  if (what %in% c("both", "buffers", "rasters")) {
    tm <- tm +
      tmap::tm_shape(points_sf) +
      tmap::tm_text("label", col = "black", size = 1,
                    xmod = 0.5, ymod = 0.5)
  }
  
  return(tm)
}


