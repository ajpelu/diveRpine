#' Landscape function
#'
#' Create a virtual landscape with different patches.
#'
#' This function creates a virtual landscape composed by:
#' * a target pine plantation
#' * natural forests
#' * crops
#' * shrublands
#'
#' The size of the pine plantation and natural forest patchs are specified
#' by @param size_pp and @param size_nf.
#'
#' First the pine plantation patch is set with the dimensions specified by
#' @param size_pp. Then a number of natural forest patchs (@param n_nf,
#' between 1 and 5) are created with a determined dimensions (@param size_nf).
#' The crop patches (a random number between 3 and 8) are then established
#' according to the remaining space. Finally, remaing pixels are assigned to
#' the shrubland category.
#'
#' @param r A \code{raster} object
#'
#' @param size_pp The size of pine plantations expressed as number of
#' pixels (\code{integer})
#'
#' @param n_nf Number of natural forest patchs to create
#' (\code{integer}, between 1 and 5)
#'
#' @param size_nf The size of natural forest patchs (\code{numeric} value,
#' between 10 and 500) expressed as number of pixels
#'
#' @return A \code{raster} object with the landscape configured
#' @examples
#' m <- matrix(nrow=100, ncol=200, byrow = TRUE)
#' r <- raster(m)
#' extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
#' r[] <- 0
#' myl <- createLandscape(r, size_pp = 1000, size_nf = 500, n_nf = 4)
#' plot(myl)
#' @import raster
#' @import landscapeR
#' @author Antonio J Pérez-Luque


createLandscape <- function(r, size_pp, n_nf, size_nf) {
  # Create pine plantation patch
  pp <- makeClass(r,
    val = 1, npatch = 1, rast = TRUE,
    size = size_pp,
    pts = matrix(c(25 * 2, 28 * 2), nrow = 1, ncol = 2)
  )

  # Create natural forest patchs
  nf <- makeClass(pp,
    val = 2, rast = TRUE,
    npatch = n_nf,
    size = size_nf
  )

  # Create crops patchs
  ### 7.5 % n cells backgroud availables
  size_c <- ceiling(ncell(nf[nf == 0]) * 0.075)
  n_crops <- sample(3:8, size = 1)

  l <- makeClass(nf,
    val = 3,
    npatch = n_crops,
    size = sample(10:size_c, size = n_crops),
    rast = TRUE
  )

  # Add factor levels (landscapes types)
  l <- ratify(l)
  rat_l <- levels(l)[[1]]
  rat_l$landuseValue <- c(0:3)
  rat_l$landuse <- c("Shrublands", "Pine plantation", "Natural Forest", "Crops")
  levels(l) <- rat_l

  return(l)
}
