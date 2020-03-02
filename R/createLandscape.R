#' Landscape function
#'
#' Create a virtual landscape with different patches.
#'
#' @param r A \code{raster} object
#'
#' @param size_pp The size of pine plantation expressed as pixel numbers
#' (\code{integer})
#'
#' @param n_nf Number of natural forest patchs to create
#' (\code{integer}, between 1 and 5)
#'
#' @param size_nf The size of natural forest patchs (\code{numeric} value,
#' between 10 and 500) expressed as number of pixels
#'
#' @return A \code{raster} object with the landscape configured
#'
#' @details
#' \itemize{This function creates a virtual landscape composed by:
#'   \item a target pine plantation
#'   \item natural forests
#'   \item crops
#'   \item shrublands
#'}
#' The size of the pine plantation and natural forest patchs are specified
#' by \code{size_pp} and \code{size_nf}.
#' First the pine plantation patch is set with the dimensions specified by
#' \code{size_pp}. Then a number of natural forest patchs (\code{n_nf},
#' between 1 and 5) are created with a determined size (\code{size_nf}).
#' The crop patches (a random number between 3 and 8) are then established
#' according to the remaining space. Finally, remain pixels are assigned to
#' the shrubland category. The size and proportion of the each patch classes
#' range according to distribution of those ecosystem in Sierra Nevada (SE
#' Spain) (see Pérez-Luque et al. 2014; 2019)
#'
#' @references
#'
#' Pérez-Luque AJ, Bonet FJ, Pérez-Pérez R, Aspizua R, Lorite J, Zamora R
#' (2014). Sinfonevada: Dataset of floristic diversity in Sierra Nevada forests
#' (SE Spain). PhytoKeys 35: 1 – 15.
#' \href{https://doi.org/10.3897/phytokeys.35.6363}{10.3897/phytokeys.35.6363}
#'
#' Pérez-Luque AJ, Bonet FJ, Zamora R (2019). Map of ecosystems types in Sierra
#' Nevada mountain (southern Spain).
#' \url{https://doi.pangaea.de/10.1594/PANGAEA.910176}
#'
#' @import raster
#' @import landscapeR
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})


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
