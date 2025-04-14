#' @name nested_buffers
#' @title Nested buffer extraction
#' @description Extracts nested buffers from a raster based on multiple radii.
#' @param r A SpatRaster object.
#' @param p A SpatVector with sampling points.
#' @param buffers Numeric vector of buffer radii.
#' @return A named list of SpatRaster objects.
#' @keywords internal
nested_buffers <- function(r, p, buffers) {
  if (!inherits(r, "SpatRaster")) stop("'r' must be a SpatRaster.")
  if (!inherits(p, "SpatVector")) stop("'p' must be a SpatVector.")
  
  buffers <- sort(unique(buffers))
  site_names <- paste0("p", sprintf("%02d", seq_along(p)))
  
  raster_list <- vector("list", length(p))
  buffer_list <- vector("list", length(p))
  names(raster_list) <- names(buffer_list) <- site_names
  
  for (i in seq_along(p)) {
    pt <- p[i]
    
    buf_geoms <- lapply(buffers, function(b) {
      g <- terra::buffer(pt, width = b)
      g$buffer_id <- paste0("buf", b)
      return(g)
    })
    
    buf_vect <- do.call(terra::vect, list(buf_geoms))
    buffer_list[[i]] <- buf_vect
    
    full_extent <- terra::ext(buf_vect)
    cropped_r <- terra::crop(r, full_extent)
    
    masked_layers <- lapply(seq_along(buf_geoms), function(j) {
      m <- terra::mask(cropped_r, buf_geoms[[j]])
      names(m) <- paste0("buf", buffers[j])
      return(m)
    })
    
    raster_list[[i]] <- terra::rast(masked_layers)
  }
  
  return(list(
    rasters = raster_list,
    buffers = buffer_list
  ))
}


#' @name decouple_buffers
#' @title Decoupled buffer extraction
#' @description Extract non-overlapping concentric rings (decoupled buffers) from a landscape raster for each sampling point.
#'   For each point, a series of outer buffers is generated, and each ring (i.e., the difference between an outer buffer and its inner neighbor)
#'   is used to mask a cropped area of the raster. The masked layers from a given point are then combined into a single SpatRaster.
#' @param r A SpatRaster object representing the landscape.
#' @param p A SpatVector of sampling points.
#' @param buffers A numeric vector of buffer radii in meters, ordered from smallest to largest.
#' @return A list with two components:
#'   \describe{
#'     \item{rasters}{A named list of SpatRaster objects (one per site) with one layer per buffer.}
#'     \item{buffers}{A named list of SpatVector objects with the buffer geometries used for each site.}
#'   }
#' @keywords internal
#' @noRd
decouple_buffers <- function(r, p, buffers) {
  if (!inherits(r, "SpatRaster")) stop("'r' must be a SpatRaster.")
  if (!inherits(p, "SpatVector")) stop("'p' must be a SpatVector.")
  if (!is.numeric(buffers) || length(buffers) < 2) {
    stop("'buffers' must be a numeric vector with at least two increasing values.")
  }
  
  buffers <- sort(unique(buffers))
  site_names <- paste0("p", sprintf("%02d", seq_along(p)))
  
  raster_list <- vector("list", length(p))
  buffer_list <- vector("list", length(p))
  names(raster_list) <- names(buffer_list) <- site_names
  
  for (i in seq_along(p)) {
    pt <- p[i]
    
    # Gere os buffers externos para o ponto
    outer_buffers <- lapply(buffers, function(b) {
      terra::buffer(pt, width = b)
    })
    
    # Cria os anéis decoplados:
    # - A primeira camada é o primeiro buffer.
    # - Para j >= 2, o anel é definido como a diferença entre o buffer atual e o buffer anterior.
    ring_geoms <- list()
    ring_geoms[[1]] <- outer_buffers[[1]]
    ring_geoms[[1]]$buffer_id <- paste0("buf", buffers[1])
    
    if (length(buffers) > 1) {
      for (j in 2:length(buffers)) {
        ring <- terra::erase(outer_buffers[[j]], outer_buffers[[j - 1]])
        ring$buffer_id <- paste0("buf", buffers[j])
        ring_geoms[[j]] <- ring
      }
    }
    
    buf_vect <- do.call(terra::vect, list(ring_geoms))
    buffer_list[[i]] <- buf_vect  # salva os buffers para este ponto
    
    full_extent <- terra::ext(buf_vect)
    cropped_r <- terra::crop(r, full_extent)
    
    masked_layers <- lapply(seq_along(ring_geoms), function(j) {
      m <- terra::mask(cropped_r, ring_geoms[[j]])
      names(m) <- paste0("buf", buffers[j])
      m
    })
    
    raster_list[[i]] <- do.call(c, masked_layers)
  }
  
  return(list(
    rasters = raster_list,
    buffers = buffer_list
  ))
}



#' @title Partially decoupled buffers
#' @description Create buffers where one focal buffer is isolated, and all others are grouped.
#' @param r A `SpatRaster`.
#' @param p A `SpatVector` of points.
#' @param buffers A numeric vector of buffer distances (sorted ascending).
#' @param keep_buffer The buffer to isolate (must be one of the values in `buffers`).
#' @return A list with `rasters` and `buffers`.
#' @export
partial_decouple_buffers <- function(r, p, buffers, keep_buffer) {
  if(is.null(keep_buffer)) stop("'buffers' is NULL but must be provided.")
  if (!is.numeric(buffers) || length(buffers) < 2) {
    stop("'buffers' must be a numeric vector with at least two values.")
  }
  if (!keep_buffer %in% buffers) {
    stop("'keep_buffer' must be one of the provided buffer distances.")
  }
  
  strategy_result <- decouple_buffers(r, p, buffers)
  raster_list <- strategy_result$rasters
  buffer_list <- strategy_result$buffers
  
  # Processar os rasters: manter apenas o buffer desejado, unir os demais
  new_rasters <- lapply(raster_list, function(rst) {
    keep_name <- paste0("buf", keep_buffer)
    all_layers <- names(rst)
    other_layers <- setdiff(all_layers, keep_name)
    
    isolated <- rst[[keep_name]]
    grouped  <- terra::mosaic(rst[[other_layers[1]]], rst[[other_layers[-1]]])
    names(isolated) <- keep_name
    names(grouped)  <- paste0("buf", paste(setdiff(buffers, keep_buffer), collapse = "_"))
    
    c(isolated, grouped)
  })
  
  # Retornar no mesmo formato
  return(list(
    rasters = new_rasters,
    buffers = buffer_list
  ))
}
