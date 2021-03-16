#' summaryRaster
#'
#' Compute custom summary stats for specific raster
#'
#' @param x A \code{raster} object
#' This function computes the mean, min, max and standard
#' deviation for a specific raster
#'
#' @return a list with mean, min, max, and sd.
#'
#' @import raster
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})
#'
#' @export
summaryRaster <- function(x) {
  require(raster)
  mean <- round(raster::cellStats(x, stat="mean"), 2)
  min <- round(raster::cellStats(x, stat="min"), 2)
  max <- round(raster::cellStats(x, stat="max"), 2)
  sd <- round(raster::cellStats(x, stat="sd"), 2)
  return(list(mean = mean,
              min = min,
              max = max,
              sd = sd))
}
