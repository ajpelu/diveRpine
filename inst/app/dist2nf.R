#' Distance to Natural forests
#'
#' Compute the distance between each pixel of the focal pine plantation and the
#' edges of all natural forests. Then get the minimum distance for each pixel.
#'
#' @param x A \code{raster} object
#'
#' @param nf_value The value of "Natural Forests" class within the raster
#' (default value = 2)
#'
#' @return A \code{raster} object with the minimum distance for each raster cell
#'
#'
#' @import raster
#' @import rgeos
#' @importFrom methods as
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})

dist2nf <- function(x, nf_value){

  require(raster)
  require(rgeos)

  # Get boundary limits of NF, and save as shapefile
  nf_edges <- raster::rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)

  # get distance between each cell(as points) and nf_edges
  dd = rgeos::gDistance(nf_edges, methods::as(x,"SpatialPoints"), byid=TRUE)

  # Get minimun distance
  dist_r <- x
  dist_r[] = apply(dd,1,min)

  return(dist_r)
}
