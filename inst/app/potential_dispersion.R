#' Compute the potential dispersion
#'
#' Compute the propagule input from each patch to focal pine plantation using
#' three classes of disperses and different kernels equations.
#'
#' @param x A \code{raster} object with the landscape configured
#'
#' @param rich_nf A \code{raster} object with richness of the natural forests
#'
#' @param nf_value The value of "Natural Forests" class within the raster
#' (default value = 2)
#'
#' @param pp_value The value of "pine plantation" class within the raster
#' (default value = 1)
#'
#' @return \code{raster} objects with values of potential dispersion for each
#' type of disperser
#' @details
#'
#' It computes the propagule input from each patch to focal pine plantation
#' using three classes of disperses and different kernels.
#' \itemize{The quantity and quality of seed dispersion are influenced by:
#'   \item Seed sources: seed diversity in seed source patch, and patch size
#'   \item Disperses: percentage of each disperser
#'   \item Landscape configuration
#'}
#' \itemize{Three classes of disperses were considered:
#'   \item small birds, \emph{e.g.} European robin (\emph{Erithacus rubecula}),
#'   Sardinian warbler (\emph{Sylvia melanocephala})
#'   \item medium birds, \emph{e.g.} Eurasian jay (\emph{Garrulus glandarius})
#'   \item mammals, \emph{e.g.} Red fox (\emph{Vulpes vulpes})
#'}
#'
#' \itemize{For each type of disperser, different dispersion kernels have been
#' considered.
#'   \item Small-sized birds rarely exceed 100 m in distance, and approximately
#'   50\% of the seeds are dispersed in the first 50 m.
#'   \item Medium-sized birds disperse 50\% of the seeds over a distance of more
#'   than 100 m. The Eurasyan jay shows a dispersion range between 5 and
#'   1000 m for Sierra Nevada mountains (SE Spain). The distance at which
#'   the maximum dispersion occurs depends on the target patch, being
#'   approximately 400 me when the target patch is a pine plantation.
#'   \item Mammals disperse in a range from 0 to more than 1500 m, with
#'   the dispersion peak at 650 - 700 m. More than 50\% of the seeds
#'   dispersed by mammals are deposited at distances greater than 495 m.
#' }
#'
#' This functions also considers the adjacency between each of the
#' natural forest patches and the focal pine plantation. It is known that the
#' higher the adjacency between the natural forest and the pine plantation, the
#' lower the limitation of the propagule entry dispersed by birds (Zamora et
#' al.2010). Then for each natural forest patch, the adjacency to pine-plantation
#' is computed. For those patches with adjacency, the potential dispersion by
#' birds increases according a factor (see Zamora et al. 2010).
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
potential_dispersion <- function(x, nf_value, pp_value, rich_nf) {

  # Output stacks
  sb <- stack()
  mb <- stack()
  ma <- stack()
  sbpot <- stack()
  mbpot <- stack()
  mapot <- stack()

  # Get perimeter of the pine plantations
  pine <- raster::rasterToPolygons(x, fun=function(x){x == pp_value}, dissolve = TRUE)
  perimeter_pine <- rgeos::gLength(pine)

  # Get polygons of Natural forests
  nf_edges <- raster::rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)
  nf_patches <- sp::disaggregate(nf_edges)

  # Operations for each polygon
  for (i in 1:length(nf_patches)) {
    # --- distance between nf polygon i and cells of the raster
    d <- rgeos::gDistance(nf_patches[i,], methods::as(x,"SpatialPoints"), byid=TRUE)
    d_nfi <- x
    d_nfi[] <- apply(d, 1, min)*10 # compute minimun distance; and multiply by 10 meters
    names(d_nfi) <- paste0("nf", i)

    # --- Adjacency
    intersectan <- st_intersects(sf::st_as_sf(nf_patches[i,]),
                                 st_as_sf(pine), sparse = FALSE)
    if(intersectan == TRUE){
    length_inter  <- st_length(
      st_intersection(sf::st_as_sf(nf_patches[i,]),
                      st_as_sf(pine), sparse = FALSE))
    } else {
      length_inter <- 0
    }

    ## Generate adj factor for nf i
    adj <- (length_inter / perimeter_pine)*100
    ## Compute the seed entry (1 - seed limit) (only for nf with intersection)
    # (seedlimit=a + b*adj) see Zamora et al.2010  a=0.7327; b = -0.0039
    seedentry <- (1 - (0.7330 - (0.0039*adj)))
    # Standardize seedentry from 0 to 1
    # 0 % adj --> 1 - (a + b*0%) = 1 - a; 0.267
    # 100 % adj --> 1 - (a + b*100); 0.657
    # std --> seedentry - 0% / (100% - 0%)
    adjF <- (seedentry - 0.267) / (0.657 - 0.267)
    fc <- 1+0.5*adjF

    # --- Richness stats (mean, se, sd ) for each nf patch
    rich_nfi <- raster::mask(rich_nf, nf_patches[i,])
    rich.mean <- raster::cellStats(rich_nfi, stat = "mean", na.rm=TRUE)
    rich.sd <- raster::cellStats(rich_nfi, stat = "sd", na.rm=TRUE)
    rich.se <- rich.sd/(sqrt(length(rich_nfi) - freq(rich_nfi, value=NA)))

    # --- Dispersion contribution
    ## Small bird
    sbi <- raster::calc(d_nfi, fun = function(x){dlnorm(x, meanlog = log(51), sdlog = .7)})
    names(sbi) <- paste0('sb',i)
    ### potential contribution
    sbipot <- stack(sbi*rich.mean*fc, sbi*rich.sd*fc, sbi*rich.se*fc)
    names(sbipot) <- c(paste0("sbpot_", i, "_mean"), paste0("sbpot_", i, "_sd"), paste0("sbpot_", i, "_se"))

    ## Medium bird
    mbi <- raster::calc(d_nfi, fun = function(x){dlnorm(x, meanlog = log(201), sdlog = .7)})
    names(mbi) <- paste0('mb',i)
    ### potential contribution
    mbipot <- stack(mbi*rich.mean*fc, mbi*rich.sd*fc, mbi*rich.se*fc)
    names(mbipot) <- c(paste0("mbpot_", i, "_mean"), paste0("mbpot_", i, "_sd"), paste0("mbpot_", i, "_se"))

    ## Mammals (not included adjacency correction)
    mai <- raster::calc(d_nfi, fun = function(x){
      ifelse(x <= 400,
             dweibull(x, shape = 1.385, scale = 137),
             dlnorm(x, meanlog = 6.621, sdlog = 0.297))})
    names(mai) <- paste0('ma',i)
    ### potential contribution
    maipot <- stack(mai*rich.mean, mai*rich.sd, mai*rich.se)
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

  out <- stack(sb, mb, ma,
               sb_all, mb_all, ma_all,
               sb_all.sd, mb_all.sd, ma_all.sd,
               sb_all.se, mb_all.se, ma_all.se)

  return(out)

}
