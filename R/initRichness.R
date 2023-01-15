#' Set values of Initial Richness
#'
#' This function computes the initial values of richness values for each patch.
#'
#' @param r A `raster` object with the configured landscape
#'
#' @param draster A `raster` object with values of the distance from the
#' target patch to natural forest patches.
#'
#' @param ftreeden,fdist functions used to computed the effect of tree
#' density (`ftreeden`), and of the distance to natural forest (`fdist`)
#' on richness values within pine plantation. See details.
#'
#' @param w_dist,w_treeden,w_past weights applied to the functions that
#' correct the plant richness values according to the distance to seed source,
#' the tree density, and the past-land use of the pine plantation.
#'
#' @param r_range A `data frame` with three columns: `value` of land use
#' (`integer`: 0 = "Other", 1 = "Pine plantation", 2 = "Natural Forests",
#' 3 = "Crop"); `lowRich` and `upRich` (lower and upper value of the
#' range of Richness)\insertCite{@see @GomezAparicio2009}{diveRpine}.
#'
#' @param treedensity density of the pine plantation (`integer`)
#'
#' @param pastUse the past land use of the pine plantation (`character`).
#' One of "Oak", "Shrubland", "Pasture" or "Crop".
#'
#' @param rescale If "TRUE" the results are rescaled (0 = min and 1 = max)
#'
#' @return A `raster` object with values of initial richness for each
#' pixel.
#'
#' @details This function computes the initial plant richness for the land-use
#' categories (*e.g.* target pine-plantation, surrounding natural
#' forests, shrubland and crops). For each land-use category, the richness value
#' of the pixels in each of the patches is randomly calculated from a range
#' of potential richness values specified by `r_range`.
#'
#' The range of plant richness values is specified by
#' `r_range`. In the diveRpine app, those values are calculated considering
#' the range of possible values found on the study area. I
#'
#'
#'
#'
#' Richness value for each of the patch classes (*i.e.* pine plantation,
#' natural forests, shrubland and crops) are calculated considering the range
#' of possible values found on the study area. In this case we use data from
#' Sierra Nevada (southern Spain)
#'
#' The richness values of each pixel of the focal (target) pine
#' patch depends on:
#' \itemize{
#'   \item Stand structure: tree density, patch size, past land-use
#'   \item Distance to seed source (landscape configuration)
#' }
#'
#'
#'
#'
#'
#' Richenss value for each of the patch classes (*i.e.* pine plantation,
#' natural forests, shrubland and crops) are calculated considering the range
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

initRichness <- function(r, draster, r_range,
                         treedensity, pastUse, elev, rad,
                         ftreeden, fdist, fclim,
                         w_past, w_treeden, w_clim, w_dist,
                         rescale = TRUE) {
  # --- Get the number of pixel for each landscape class
  ncell_pp <- ncell(r[r == 1])
  ncell_nf <- ncell(r[r == 2])
  ncell_crop <- ncell(r[r == 3])

  # --- Set the Potential Richness values

  if (missing(r_range)) {
    r_range <- as.data.frame(
      cbind(
        value = c(0, 1, 2, 3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
        upRich = c(0, 13.34, mean(16.11, 19.66), 2)
      )
    )
  } else {
    r_range
  }

  ## Ranges
  range_pp <- r_range[which(r_range$value == 1), ]
  range_nf <- r_range[which(r_range$value == 2), ]
  range_crop <- r_range[which(r_range$value == 3), ]

  ## Potential vectors
  potR_pp <- runif(ncell_pp * 3, range_pp$lowRich, range_pp$upRich)
  potR_nf <- runif(ncell_nf * 3, range_nf$lowRich, range_nf$upRich)
  potR_crop <- runif(ncell_crop * 3, range_crop$lowRich, range_crop$upRich)

  # --- Reclassify
  r[r == 0] <- NA
  r[r == 1] <- -100
  r[r == 2] <- -200
  r[r == 3] <- -300

  # --- Pine plantation
  ## ~ Tree Density
  ### Fraction of Potential Richness (Default to tree-density Eq3 from
  ### Gómez-Aparicio et al. 2009).
  if (missing(ftreeden)) {
    ftreeden <- exp(-0.5 * ((treedensity - 0.22) / 1504.1)^2)
  } else {
    ftreeden
  }

  ## ~ Climatic effect
  ### Fraction of Potential Richness (Default to tree-density Eq2 from
  ### Gómez-Aparicio et al. 2009).
  if (missing(fclim)) {
    felev <- exp(-0.5 * ((elev - 1557.16) / 644.89)^2)
    frad <- exp(-0.5 * ((rad - 0) / 13.24)^2)
    fclim <- frad * felev

  } else {
    fclim
  }

  ## ~ Distance to Seed Source
  ### Compute diversity raster (see Gonzalez-Moreno et al. 2011)
  if (missing(fdist)) {
    fdist <- function(x) {
      1.7605 - 0.0932 * (sqrt(sqrt(x)))
    }
  } else {
    fdist
  }

  sh <- calc(draster, fun = fdist)

  ### Create a stack with the shannon diversity raster and landuse raster,
  ### and then compute values for pine plantations
  s <- calc(stack(r, sh), fun = function(x) ifelse(x[1] == -100, (x[1] / -100) * x[2], NA))

  ### Scale the distance effect from 0 to 1
  sh_scaled <- (s - cellStats(s, "min")) / (cellStats(s, "max") - cellStats(s, "min"))

  ## ~ Past Use
  ### Past Land Use
  fplu <- ifelse(pastUse == "Oak", .9999,
    ifelse(pastUse == "Shrubland", .4982,
      ifelse(pastUse == "Crop", .0279, .0001)
    )
  )

  ## Combine factor to correct pine plantations
  ### set the weights values according to literature review. See references
  if (missing(w_past)) {
    w_past <- 0.2
  } else {
    w_past
  }

  if (missing(w_treeden)) {
    w_treeden <- 0.25
  } else {
    w_treeden
  }

  if (missing(w_clim)) {
    w_clim <- 0.2
  } else {
    w_clim
  }

  if (missing(w_dist)) {
    w_dist <- 0.35
  } else {
    w_dist
  }



  ### Apply weigths
  f_pine <- (sh_scaled * w_dist) + (w_treeden * ftreeden) + (w_clim * fclim) + (w_past * fplu)

  r[r == -100] <- sample(potR_pp, ncell_pp, replace = TRUE)
  r <- calc(stack(r, f_pine), fun = function(x) ifelse(x[1] < -100, x[1], x[1] * x[2]))

  # --- Crops
  r[r == -300] <- sample(potR_crop, ncell_crop, replace = TRUE)

  # --- Natural forest
  r[r == -200] <- sample(potR_nf, ncell_nf, replace = TRUE)

  # Rescale results
  if (rescale) {
    r <- (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min"))
  }

  return(r)
}
