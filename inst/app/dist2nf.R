#' Distance to Natural forests
#'
#' `dist2nf` computes the distance between the target pine plantation and
#' all surroundings natural forests patches.
#'
#' This auxiliary function calculates the distance for each pixel of the target
#' pine plantation to the edges of all surrounding natural forest patches. For
#' each pixel, the final value considered is the minimum of all distances
#' from that pixel to the edges of natural forests.
#'
#' @param x A `raster` object
#'
#' @param nf_value The value of "Natural Forests" landscape class within the
#' raster (default value = 2).
#'
#' @return A `raster` object with the minimum distance for each raster pixel
#'
#'
#' @import raster
#' @import rgeos
#' @importFrom methods as
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})

dist2nf <- function(x, nf_value) {
  if (missing(nf_value)) {
    nf_value <- 2
  } else {
    nf_value
  }

  # Get boundary limits of NF, and save as polygon
  nf_edges <- raster::rasterToPolygons(x, fun = function(x) {
    x == nf_value
  }, dissolve = TRUE)

  # get distance between each pixel(as points) and nf_edges
  dd <- rgeos::gDistance(nf_edges, methods::as(x, "SpatialPoints"), byid = TRUE)

  # Get minimum distance
  dist_r <- x
  dist_r[] <- apply(dd, 1, min)

  return(dist_r)
}
