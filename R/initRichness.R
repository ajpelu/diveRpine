#' Set values of Initial Richness
#'
#' This function computes the initial values of richness values for each patch.
#'
#' @param r A `raster` object with the configured landscape
#'
#' @param draster A `raster` object with values of the distance from the
#' target patch to natural forest patches.
#'
#' @param ftreeden,fdist,fclim functions used to computed the effect of tree
#' density (`ftreeden`), and of the distance to natural forest (`fdist`), and of
#' the climate-proxy variables (`fclim`) on richness values within pine plantation.
#' See details.
#'
#' @param w_dist,w_treeden,w_past,w_clim weights applied to the functions that
#' correct the plant richness values according to the distance to seed source,
#' the tree density, the past-land use of the pine plantation, and also the
#' climate-proxy variables of the pine plantation.
#'
#' @param r_range A `data frame` with three columns: `value` of land use
#' (`integer`: 0 = "Other", 1 = "Pine plantation", 2 = "Natural Forests",
#' 3 = "Crop"); `lowRich` and `upRich` (lower and upper value of the
#' range of Richness).
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
#' of potential richness values specified by `r_range`. In diveRpine, richness
#' value for each of the patch classes are calculated considering the range
#' of possible values found on the study area (Sierra Nevada, southern Spain).
#' Specifically:
#' | value| lowRich| upRich|
#' |-----:|-------:|------:|
#' |     No Data |    0.00|   0.00|
#' |     Pine Plantations |   12.82|  13.34|
#' |     Natural Forests |   13.72|  16.11|
#' |     Crops |    1.00|   2.00|
#'
#' A richness value is assigned to each pixel. This value will depend on the
#' pixel category.
#'
#' ## Pine plantation
#' The richness values of each pixel of the focal (target) pine
#' patch depends on:
#' \itemize{
#'   \item Stand Features: tree density, patch size, past land-use, climate-proxy variables
#'   \item Distance to seed source (landscape configuration)
#' }
#' For each pixel *j*, the initial richness value (\eqn{R_{init,j}}) is computed
#' as \deqn{Richess \sim Potential\ Richenss \times fc}
#' where *Potential Richenss* is a random value coming from `r_range` and *fc*
#' is a correction factor:
#' \deqn{fc = w_{past}\cdot f(\textrm{past~Land~Use}) + w_{dist}\cdot f(\textrm{Seed~source~distance}) + w_{treeden}\cdot f(\textrm{Tree~density}) + w_{clim}\cdot f(\textrm{Climate~proxy})}
#'
#' We specified the following weights according to literature (see references):
#
#' - \eqn{w_{past}} = 0.2
#' - \eqn{w_{dist}} = 0.35
#' - \eqn{w_{treeden}} = 0.25
#' - \eqn{w_{clim}} = 0.2
#'
#' but different weights can be provided using `w_past`, `w_dist`,`w_treeden`,
#' and `w_clim` respectively.
#'
#' Each of the factors affecting the richness within a pine plantation pixel are
#' computed as follows:
#'
#' ### Tree density (`ftreeden`)
#' Richness and species diversity within pine plantation are strongly conditioned
#' by tree density, which has a negative effect on the plant diversity, and on
#' the total plant species richness. Potential richness is affected as a function
#' of density, as follows:
#' \deqn{\textrm{ftreeden} = \exp \left(-\frac{1}{2} \left( \frac{ \textrm{treeDensity} - 0.22} {1504.1}\right )^2\right )}
#'
#' This equation is the used by default, but could be change with `ftreeden`. Tree
#' density value is specified by `treedensity`.
#'
#' ### Climate-proxy factors
#' Potential richness are also strongly affected by climate-proxy variables.
#' It has been determined from 19 climatic and topographical variables
#' that the elevation and the annual radiation are the variables that best
#' explaining the variability of potential richness within pine-plantation
#' (they capturing more than 83.3 % of the observed variance). The climate effect
#' on potential richness within pine-plantation has been modeled according to the
#' following equation:
#' \deqn{\textrm{fclim}=\textrm{exp}\left (-\frac{1}{2}\left ( \frac{\textrm{Altitude}- 1557.16}{644.89} \right )^{2}  \right ) \times ~\textrm{exp}\left (-\frac{1}{2}\left ( \frac{\textrm{Radiation}}{13.24} \right )^{2}  \right )}
#'
#' This equation is the used by default, but could be change with `fclim`. Altitude
#' and radiation values are specified by `elev` and `rad`.
#'
#' ### Seed source distance (`fdist`).
#' Seed dispersal depends on the distance from the seed source. In pine plantations,
#' the presence and abundance of species other than pines is determined, among
#' others, by the distance to the seed source. In Sierra Nevada (southern Spain)
#' natural oak forests are the most influential in terms of distance to the seed
#' source. Oak vegetation has higher plant diversity than pine plantations,
#' especially for herbaceous species. Shorter distances could increase the pool
#' of species in the pine plantations and reduce the evenness of plantation
#' communities. The relationship found between distance to the source and diversity
#' observed in pine plantations is governed by the following equation:
#' \deqn{\textrm{Diversity} = 1.7605 - 0.0932 * \sqrt{\sqrt{\textrm{Distance}}}}
#'
#' This equation is the used by default, but could be change with `fdist`. For
#' each pixel of pine plantation the distances between the centroid of the pixel
#' and the edge of each natural forest patches are computed using the function
#' [diveRpine::dist2nf()] which generate a distance raster (`draster`).
#'
#' ### Past Land Use
#' The richness value of a plantation is conditioned by the past land use. For
#' instance, it has been found that regeneration of *Quercus* in pine
#' plantations depends more on past land-use than on plantation tree density and
#' distance to the seed source. Navarro-González et al. (2013), found that the
#' probability finding regeneration within a plantation varies as a function of
#' past land use. We rescaled the gradient found by Navarro-González et al. (2013)
#' as follow:  natural forest (0.9999), Shrubland (0.4982), Cropland (0.0279),
#' and Grassland (0.0001). The value of \eqn{f(\textrm{past Land Use})} is selected
#' according to the past land use specified in `pastUse`.
#'
#' ## Natural forests and Croplands
#' The initial richness values of each pixel of natural forest and cropland patches are
#' randomly selected from the value ranges specified on `r_range`.
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
