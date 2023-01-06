#' create_landscape
#'
#' Create an empty landscape as raster with a default value
#'
#' @param width,height dimension of the raster (ncells). Default values:
#' \code{width} = 126 and \code{height} = 106.
#'
#' @param value default value set for the pixels (\code{value} = 0)
#'
#' @return raster Object
#' @export
#'
#'
#' @import raster
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})
#'
create_landscape <- function(width, height, value) {

  # Size of the landscape (adimensional pixels)
  if (missing(width)) {
    width <- 63 * 2
  } else {
    width
  }

  if (missing(height)) {
    height <- 53 * 2
  } else {
    height
  }

  # Create an empty landscape
  set.seed(123)
  m <- matrix(nrow = height, ncol = width, byrow = T)
  empty_landscape <- raster::raster(m)

  # Set the extent of raster
  extent(empty_landscape) <- matrix(c(0, 0, width, height), nrow = 2)

  # Assing value = 0 to all pixels of the raster created
  if (missing(value)) {
    empty_landscape[] <- 0
  } else {
    empty_landscape[] <- value
  }

  return(empty_landscape)
}
