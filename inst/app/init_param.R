# Load pkgs
library(raster)

## -------------------------------------------------
### Created an empty landscape
set.seed(123)

# size
ancho <- 63 * 2 # n cols
alto <- 53 * 2 # n rows

m <- matrix(nrow=alto, ncol=ancho, byrow = T)
empty_landscape <- raster(m)
# Define extent of raster
extent(empty_landscape) <- matrix(c(0, 0, ancho, alto), nrow=2)
empty_landscape[] <- 0
## -------------------------------------------------

## -------------------------------------------------
### Position for target_plantation
position_pine <- matrix(c(nrow(empty_landscape) / 2,
                          ncol(empty_landscape) / 2),
                        ncol = 2, nrow = 1)
## ------------------------------------------------

## -------------------------------------------------
### Potential number of crops patches
n_crops <- sample(3:8, size = 1)
## -------------------------------------------------

## -------------------------------------------------
## Some parameters
line_pol <- 2 ### Line width polygon
pp_value <- 1 ### Value for Pine plantation
nf_value <- 2 ### Value for Natural forest
h_plots <- 700 ### Set height plots (for dashboard)
## -------------------------------------------------

## -------------------------------------------------
# Richness range for each land-use (see package references)
ri_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
        upRich = c(0, 13.34, mean(16.11, 19.66), 2)))
## -------------------------------------------------

## ---------------------------------------------------
# Propagule input density (numbers of propagule / m2 year) (see package references)
piBird = (3.7)/100
piMammal = (0.2)/100
