#' Compute the potential dispersion
#'
#' Compute the propagule input from each patch to target pine plantation using
#' three classes of disperses and different kernels equations.
#'
#' @param x A `raster` object with the landscape configured
#'
#' @param rich_nf A `raster` object with richness of the natural forests
#'
#' @param nf_value The value of "Natural Forests" class within the raster
#' (default value = 2).
#'
#' @param pp_value The value of "pine plantation" class within the raster
#' (default value = 1).
#'
#' @param kernel_sbi,kernel_mbi,kernel_ma The dispersion kernel functions for
#' small birds (`kernel_sbi`), medium birds (`kernel_mbi`) and mammals (`kernel_ma`).
#' See details.
#'
#' @param seedlim_int,seedlim_slope The intercept and slope of the linear relation
#' between the seed limitation in the target plantation and the adjacency of a
#' natural forest patch with the target pine plantation. See details
#'
#' @return `raster` objects with values of potential dispersion for each
#' type of disperser
#'
#'
#' @details
#'
#' It computes the propagule input from each patch to focal pine plantation
#' using three classes of disperses and different kernels. The quantity and
#' quality of seed dispersion are influenced by:
#'
#' - Seed sources: seed diversity in seed source patch, and patch size
#'
#' - Disperses: percentage of each disperser type
#'
#' - Landscape configuration
#'
#' Three classes of disperses were considered by default for the study
#' area:
#'
#' - small birds, *e.g.* European robin (*Erithacus rubecula*), Sardinian warbler
#' (*Sylvia melanocephala*)
#'
#' - medium birds, *e.g.* Eurasian jay (*Garrulus glandarius*)
#'
#' - mammals, *e.g.* Red fox (*Vulpes vulpes*)
#'
#' For each type of disperser, different dispersion kernels have been
#' considered:
#'
#' - Small-sized birds rarely exceed 100 m in distance, and approximately
#'   50% of the seeds are dispersed in the first 50 m.
#'
#' - Medium-sized birds disperse 50% of the seeds over a distance of more
#'   than 100 m. The Eurasyan jay shows a dispersion range between 5 and 1000 m
#'   for Sierra Nevada mountains (SE Spain). The distance at which the maximum
#'   dispersion occurs depends on the target patch, being approximately 400 m
#'   when the target patch is a pine plantation.
#'
#' - Mammals disperse in a range from 0 to more than 1500 m, with
#'   the dispersion peak at 650 - 700 m. More than 50% of the seeds
#'   dispersed by mammals are deposited at distances greater than 495 m.
#'
#' According to the disperser type, the function uses by default different
#' dispersion kernels. For small and medium birds a log-normal dispersion kernels
#' were used. Specifically, log-normal density function with mean and standard
#' deviation values of log(51) and log(2) respectively for small-sized birds,
#' and log-normal density function with mean and standard deviation values of
#' log(201) and log(2) for medium-sized birds. This distribution were implemented
#' using the [stats::dlnorm()] function. For mammals, a combination of log-normal
#' and Weibull dispersion kernel was used. Specifically, from 0 to 400 m of distance
#' from the seed source, a Weibull distribution with a shape and scale parameters
#' of 1.385 and 137 respectively was used (See [stats::dweibull()]); whereas a
#' log-normal density function with mean and standard deviation values of log(751)
#' and log(1.346) respectively, were applied for distances higher than 401 m
#' from seed source. Different kernels functions could be specified using the
#' parameters `kernel_sbi`, `kernel_mbi`, and `kernel_ma`.
#'
#' This function also considers the adjacency between each of the natural forest
#' patches and the target pine plantation. The higher the adjacency between the
#' natural forest and the pine plantation, the lower the limitation of the propagule
#' entry dispersed by birds. Zamora et al. (2010) found that the intercept and the
#' slope of the linear relation between the seed limitation (values from 0 to 1)
#' and the adjacency (*i.e.* percentage of pine-plantation perimeter in contact
#' with native forests) were 0.733 and 0.0039 respectively. Those parameters are
#' included by default, but could be customized using the `seedlim_int` and
#' `seedlim_slope` parameters. For each natural forest patch, the adjacency to
#' target pine-plantation is computed. For those patches with adjacency, the
#' potential dispersion by birds increases according a correction factor (see
#' Zamora et al. 2010), that is computed as follows:
#'
#' \eqn{adj_{fc} = 1 + \frac{\textrm{seed Entry} - \textrm{seed Entry}_{0}}{\textrm{seed Entry}_{100} - \textrm{seed Entry}_{0}}}
#'
#'
#' where \eqn{\textrm{seed Entry} = 1- \textrm{seed limitation}}; \eqn{\textrm{seed limitation}}
#' is computed using the `seedlim_int` and `seedlim_slope` parameters;
#' \eqn{\textrm{seed Entry}_{0}} and \eqn{\textrm{seed Entry}_{100}} correspond
#' to the seed entry for no adjacency and full adjacency respectively (see
#' Zamora et al. 2010).
#'
#' @references
#' \insertRef{Gomez2003}{diveRpine}
#'
#' \insertRef{GonzalezVaro2013}{diveRpine}
#'
#' \insertRef{Jordano2007}{diveRpine}
#'
#' \insertRef{Matias2010}{diveRpine}
#'
#' \insertRef{Pons2007}{diveRpine}
#'
#' \insertRef{Zamora2010}{diveRpine}
#'
#' @import raster
#' @import rgeos
#' @import sp
#' @import sf
#' @importFrom stats dlnorm dweibull
#' @importFrom methods as
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})
#' @export
potential_dispersion <- function(x, nf_value, pp_value, rich_nf,
                                 kernel_sbi, kernel_mbi, kernel_ma,
                                 seedlim_int, seedlim_slope) {
  if (missing(nf_value)) {
    nf_value <- 2
  } else {
    nf_value
  }

  if (missing(pp_value)) {
    pp_value <- 1
  } else {
    pp_value
  }

  # Kernels
  if (missing(kernel_sbi)) {
    kernel_sbi <- function(x) {
      dlnorm(x, meanlog = log(51), sdlog = log(2))
    }
  } else {
    kernel_sbi
  }

  if (missing(kernel_mbi)) {
    kernel_mbi <- function(x) {
      dlnorm(x, meanlog = log(201), sdlog = log(2))
    }
  } else {
    kernel_mbi
  }

  if (missing(kernel_ma)) {
    kernel_ma <- function(x) {
      ifelse(x <= 400,
        dweibull(x, shape = 1.385, scale = 137),
        dlnorm(x, meanlog = 6.621, sdlog = 0.297)
      )
    }
  } else {
    kernel_ma
  }

  # Seed Limit
  if (missing(seedlim_int)) {
    seedlim_int <- 0.733
  } else {
    seedlim_int
  }

  if (missing(seedlim_slope)) {
    seedlim_slope <- 0.0039
  } else {
    seedlim_slope
  }

  # Output stacks
  sb <- stack()
  mb <- stack()
  ma <- stack()
  sbpot <- stack()
  mbpot <- stack()
  mapot <- stack()

  # Get perimeter of the pine plantations
  pine <- raster::rasterToPolygons(x, fun = function(x) {
    x == pp_value
  }, dissolve = TRUE)
  perimeter_pine <- rgeos::gLength(pine)

  # Get polygons of Natural forests
  nf_edges <- raster::rasterToPolygons(x, fun = function(x) {
    x == nf_value
  }, dissolve = TRUE)

  nf_edges <- as(sf::st_as_sf(nf_edges), "Spatial")
  nf_patches <- sp::disaggregate(nf_edges)

  # Operations for each polygon
  for (i in 1:length(nf_patches)) {
    # --- distance between nf polygon i and cells of the raster
    d <- rgeos::gDistance(nf_patches[i, ], methods::as(x, "SpatialPoints"), byid = TRUE)
    d_nfi <- x
    d_nfi[] <- apply(d, 1, min) * 10 # compute minimum distance; and multiply by 10 meters
    names(d_nfi) <- paste0("nf", i)

    # --- Adjacency
    intersectan <- sf::st_intersects(sf::st_as_sf(nf_patches[i, ]),
      sf::st_as_sf(pine),
      sparse = FALSE
    )
    if (intersectan == TRUE) {
      length_inter <- sf::st_length(
        sf::st_intersection(sf::st_as_sf(nf_patches[i, ]),
          sf::st_as_sf(pine),
          sparse = FALSE
        )
      )
    } else {
      length_inter <- 0
    }

    ## Generate adj factor for nf i
    adj <- (length_inter / perimeter_pine) * 100
    ## Compute the seed entry (1 - seed limit) (only for nf with intersection)
    # (seedlimit=a + b*adj) see Zamora et al.2010  a=0.7327; b = -0.0039
    seedentry <- (1 - (seedlim_int - (seedlim_slope * adj)))
    # Standardize seedentry from 0 to 1
    # 0 % adj --> 1 - (a + b*0%) = 1 - a; 0.267
    # 100 % adj --> 1 - (a + b*100); 0.657
    # std --> seedentry - 0% / (100% - 0%)
    adjF <- (seedentry - 0.267) / (0.657 - 0.267)
    fc <- 1 + 0.5 * adjF

    # --- Richness stats (mean, se, sd ) for each nf patch
    rich_nfi <- raster::mask(rich_nf, nf_patches[i, ])
    rich.mean <- raster::cellStats(rich_nfi, stat = "mean", na.rm = TRUE)
    rich.sd <- raster::cellStats(rich_nfi, stat = "sd", na.rm = TRUE)
    rich.se <- rich.sd / (sqrt(length(rich_nfi) - freq(rich_nfi, value = NA)))

    # --- Dispersion contribution
    ## Small bird
    sbi <- raster::calc(d_nfi, fun = kernel_sbi)
    names(sbi) <- paste0("sb", i)
    ### potential contribution
    sbipot <- stack(sbi * rich.mean * fc, sbi * rich.sd * fc, sbi * rich.se * fc)
    names(sbipot) <- c(paste0("sbpot_", i, "_mean"), paste0("sbpot_", i, "_sd"), paste0("sbpot_", i, "_se"))

    ## Medium bird
    mbi <- raster::calc(d_nfi, fun = kernel_mbi)
    names(mbi) <- paste0("mb", i)
    ### potential contribution
    mbipot <- stack(mbi * rich.mean * fc, mbi * rich.sd * fc, mbi * rich.se * fc)
    names(mbipot) <- c(paste0("mbpot_", i, "_mean"), paste0("mbpot_", i, "_sd"), paste0("mbpot_", i, "_se"))

    ## Mammals (not included adjacency correction)
    mai <- raster::calc(d_nfi, fun = kernel_ma)
    names(mai) <- paste0("ma", i)
    ### potential contribution
    maipot <- stack(mai * rich.mean, mai * rich.sd, mai * rich.se)
    names(maipot) <- c(paste0("mapot_", i, "_mean"), paste0("mapot_", i, "_sd"), paste0("mapot_", i, "_se"))

    sb <- stack(sb, sbi)
    mb <- stack(mb, mbi)
    ma <- stack(ma, mai)

    sbpot <- stack(sbpot, sbipot)
    mbpot <- stack(mbpot, mbipot)
    mapot <- stack(mapot, maipot)
  }

  # Get values of sb, mb, ma for all nf
  sb_all <- sum(raster::subset(sbpot, grep("^sbpot.+_mean$", names(sbpot), value = T)))
  names(sb_all) <- "sb"
  mb_all <- sum(raster::subset(mbpot, grep("^mbpot.+_mean$", names(mbpot), value = T)))
  names(mb_all) <- "mb"
  ma_all <- sum(raster::subset(mapot, grep("^mapot.+_mean$", names(mapot), value = T)))
  names(ma_all) <- "ma"

  sb_all.sd <- sum(raster::subset(sbpot, grep("^sbpot.+_sd$", names(sbpot), value = T)))
  names(sb_all.sd) <- "sb.sd"
  mb_all.sd <- sum(raster::subset(mbpot, grep("^mbpot.+_sd$", names(mbpot), value = T)))
  names(mb_all.sd) <- "mb.sd"
  ma_all.sd <- sum(raster::subset(mapot, grep("^mapot.+_sd$", names(mapot), value = T)))
  names(ma_all.sd) <- "ma.sd"

  sb_all.se <- sum(raster::subset(sbpot, grep("^sbpot.+_se$", names(sbpot), value = T)))
  names(sb_all.se) <- "sb.se"
  mb_all.se <- sum(raster::subset(mbpot, grep("^mbpot.+_se$", names(mbpot), value = T)))
  names(mb_all.se) <- "mb.se"
  ma_all.se <- sum(raster::subset(mapot, grep("^mapot.+_se$", names(mapot), value = T)))
  names(ma_all.se) <- "ma.se"

  out <- stack(
    sb, mb, ma,
    sb_all, mb_all, ma_all,
    sb_all.sd, mb_all.sd, ma_all.sd,
    sb_all.se, mb_all.se, ma_all.se
  )

  return(out)
}
