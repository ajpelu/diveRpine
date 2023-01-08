#' Seed input propagules into pine plantation
#'
#' Compute the propagule input into target pine plantation.
#'
#' @param x A `raster` object with the landscape configured
#
#' @param pd A `raster stack` object with raster from the potential
#' dispersion. See `potential_dispersion` function.
#'
#' @param pp_value The value of "pine plantation" class within the raster
#' (default value = 1).
#'
#' @details This auxiliary function masks the raster generated from the
#' `potential_dispersion` function with the limits of the target pine plantation.
#'
#'
#' @return raster Object
#'
#' @import raster
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})
#'
#' @export

input_propagule <- function(x, pd, pp_value) {
  if (missing(pp_value)) {
    pp_value <- 1
  } else {
    pp_value
  }

  pp <- raster::rasterToPolygons(x, fun = function(x) {
    x == pp_value
  }, dissolve = TRUE)
  propagules_pp <- raster::mask(pd, pp)
  return(propagules_pp)
}
