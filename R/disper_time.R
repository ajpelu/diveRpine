#' Propagule input into pine plantation by time
#'
#' It computes the seeds input into focal pine plantation along the time.
#'
#' @details
#' Compute the propagule input from each natural forest patch to pine
#' plantation target using three types of disperser and different equations.
#' The seed input into focal pine plantation depends on landscape configuration:
#' distance to the seed source and quantity and typology of vegetation patches;
#' and on the composition of the dispersal community. Data about amount of seed
#' introduced into pine plantation in Sierra Nevada mountain (SE Spain) comes
#' from \insertCite{@ @Zamora2010}{diveRpine}.
#'
#' @param x A \code{raster} object with the landscape configured
#'
#' @param xr A \code{raster} object with richness computed
#'
#' @param msb A \code{raster} object with small bird contribution on target
#' pine plantation
#'
#' @param mmb A \code{raster} object with medium bird contribution on target
#' pine plantation
#'
#' @param mma A \code{raster} object with mammal contribution on target
#' pine plantation
#'
#' @param pp_value The value of "Pine plantation" class within the raster
#' (Default value = 1)
#'
#' @param per_sb Percentage of small bird dispersers
#'
#' @param per_mb Percentage of medium bird dispersers
#'
#' @param per_ma Percentage of mammals dispersers
#'
#' @param propaguleInputBird Propagule input by birds (seeds number by year and
#' square meter) \insertCite{@see @Zamora2010}{diveRpine}
#'
#' @param propaguleInputMammal Propagule input by mammals (seeds number by year and
#' square meter) \insertCite{@see @Zamora2010}{diveRpine}
#'
#' @param time_span Numbers of year simulated (maximum 50)
#'
#' @references
#' \insertRef{Zamora2010}{diveRpine}
#'
#' @import raster
#' @import rgeos
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})

disper_time <- function(msb, mmb, mma, x, xr, pp_value, per_sb,
                        per_mb, per_ma, propaguleInputBird,
                        propaguleInputMammal,time_span){

  # Output stack
  out <- stack()

  # Get richnes of pine plantations
  rich_pp <- calc(stack(x, xr), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))

  # Compute propagule input by cell
  seed_input <- ((msb * per_sb) + (mmb * per_mb)) * propaguleInputBird  + (mma * per_ma) * propaguleInputMammal

  for (i in 1:time_span){
    aux <- c()
    propagulo_time <- rich_pp + (seed_input)*i
    names(propagulo_time) <- paste0('y',i)


    rich_time <- calc(stack(x,xr, propagulo_time),
                      fun = function(x) ifelse(
                        x[1] == pp_value, x[1]*x[3], x[2]))

    names(rich_time) <- paste0('rich_y',i)
    rich_time[rich_time == 0] <- NA

    out <- stack(out, rich_time)}

  return(out)
}

