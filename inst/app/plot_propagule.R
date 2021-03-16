#' plot_propagule
#'
#' Plot the input propagules into the focal pine plantation
#'
#' @param x A \code{raster} object
#'
#' @return ggplot2 Object
#'
#' @import ggplot2
#' @import raster
#' @importFrom Rdpack reprompt
#' @author Antonio J Pérez-Luque (\email{ajpelu@@gmail.com})
#'
#' @export
plot_propagule <- function(x,
                          ...){
  # derive ratio for plot, cells should be a square and axis equal in length
  if (raster::ncol(x) == raster::nrow(x)) {
    ratio <- 1
  } else {
    ratio <- raster::nrow(x) / raster::ncol(x)
  }

  xyz <- raster::as.data.frame(x, xy=TRUE)
  names(xyz) <- c("x","y","seed")

  ggplot2::ggplot(xyz) +
    ggplot2::geom_raster(ggplot2::aes(x, y, fill = seed)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(color = "black"),
      legend.key.width = unit(2.5,"cm"),
      aspect.ratio = ratio,
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.border = ggplot2::element_rect(fill = NA,
                                           colour = "black", size = 1),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")
    ) +
    ggplot2::scale_fill_distiller(palette = "YlOrBr",
                                  type = "seq", direction = 1, na.value = "transparent") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))
}
