#' Set values of Initial Richness
#'
#' This function computes the initial values of richness values for each patch.
#'
#' @param r A \code{raster} object with the configured landscape
#'
#' @param draster A \code{raster} object with values of the distance from the
#' target patch to natural forest patchs
#'
#' @param r_range A \code{data frame} with three columns: \code{value} of land use
#' (\code{integer}: 0 = "Other", 1 = "Pine plantation", 2 = "Natural Forests",
#' 3 = "Crop"); \code{lowRich} and \code{upRich} (lower and upper value of the
#' range of Richness)\insertCite{@see @GomezAparicio2009}{diveRpine}.
#'
#' @param treedensity density of the pine plantation (\code{integer})
#'
#' @param pastUse the past land use of the pine plantation (\code{character}).
#' One of "Oak", "Shrubland", "Pasture" or "Crop"
#'
#' @param rescale If "TRUE" the results are rescaled (0 = min and 1 = max)
#'
#' @return A \code{raster} object with values of initial richness for each
#' pixel.
#'
#' @details This function computes the initial richness for each land-use
#' categories. The richness values of each pixel of the focal (target) pine
#' patch depends on:
#' \itemize{
#'   \item Stand structure: tree density, patch size, past land-use
#'   \item Distance to seed source (landscape configuration)
#' }
#'
#' Richness value for each of the patch classes (\emph{i.e.} pine plantation,
#' natural forests, shrublands and crops) are calculated considering the range
#' of possible values found on the study area. In this case we use data from
#' Sierra Nevada (southern Spain) See References.
#'
#' @references
#' \insertRef{GomezAparicio2009}{diveRpine}
#'
#' \insertRef{Mendoza2009}{diveRpine}
#'
#' \insertRef{GonzalezMoreno2011}{diveRpine}
#'
#' \insertRef{NavarroGonzalez2013}{diveRpine}
#'
#' \insertRef{PerezLuque2014}{diveRpine}
#'
#' @import raster
#' @importFrom stats runif
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})

initRichness <- function(r, draster, r_range, treedensity, pastUse, rescale=TRUE){

  # --- N cells
  ncell_pp <- raster::ncell(r[r == 1])
  ncell_nf <- raster::ncell(r[r == 2])
  ncell_crop <- raster::ncell(r[r == 3])

  # --- Potential Richness values
  ## Ranges
  range_pp <- r_range[which(r_range$value == 1), ]
  range_nf <- r_range[which(r_range$value == 2), ]
  range_crop <- r_range[which(r_range$value == 3), ]

  ## Potential vectors
  potR_pp <- runif(ncell_pp*3, range_pp$lowRich, range_pp$upRich)
  potR_nf <- runif(ncell_nf*3, range_nf$lowRich, range_nf$upRich)
  potR_crop <- runif(ncell_crop*3, range_crop$lowRich, range_crop$upRich)

  # --- Reclassify
  r[r == 0] <- NA
  r[r == 1] <- -100
  r[r == 2] <- -200
  r[r == 3] <- -300

  # --- Pine plantation
  ## ~ TreeDensity
  ### Fraction of Potential Richness (tree-density
  ### Eq. 3 Gómez-Aparicio et al. 2009)
  ftreeden <- exp(-0.5*((treedensity - 0.22)/1504.1)^2)

  ## ~ Distance to Seed Source
  ### Compute diversity raster (See Gonzalez-Moreno et al. 2011)
  sh <- raster::calc(draster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})

  ### Create a stack with the shanon diversity raster and landuse raster,
  ### and then compute values for pine plantations
  s <- raster::calc(stack(r, sh), fun=function(x)  ifelse(x[1] == -100 , (x[1]/-100)*x[2],  NA))

  ### Scale the distance effect from 0 to 1
  sh_scaled <- (s - cellStats(s, "min"))/(cellStats(s, "max") - cellStats(s, "min"))

  ## ~ PastUSE
  ### Past Land Use
  fplu <- ifelse(pastUse == 'Oak', .9999,
                 ifelse(pastUse == 'Shrubland', .4982,
                        ifelse(pastUse == 'Crop', .0279, .0001)))

  ## Combine factor to correct pine plantations
  f_pine <- (sh_scaled*0.35) + (.45*ftreeden + .2*fplu)

  r[r == -100]  <- sample(potR_pp, ncell_pp, replace = TRUE)
  r <- calc(stack(r, f_pine), fun = function(x) ifelse(x[1] < -100, x[1], x[1]*x[2]))

  # --- Crops
  r[r == -300]  <- sample(potR_crop, ncell_crop, replace = TRUE)

  # --- Natural forest
  r[r == -200]  <- sample(potR_nf, ncell_nf, replace = TRUE)

  # Rescale results
  if (rescale)
    r <- (r - raster::cellStats(r, "min"))/(raster::cellStats(r, "max") - raster::cellStats(r, "min"))

  return(r)

}
