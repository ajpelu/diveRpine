# createLandscape --> Good

size_pp = 1000
size_nf = 500
n_nf = 4


m <- matrix(nrow=100, ncol=200, byrow = TRUE)
r <- raster(m)
extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
r[] <- 0



myl <- createLandscape(r, size_pp = 1000, size_nf = 500, n_nf = 4)

plot(myl)


###
library(raster)
createPine <- function(r, size_pp) {
  # Create pine plantation patch
  pp <- makeClass(r,
                  val = 1, npatch = 1, rast = TRUE,
                  size = size_pp,
                  pts = matrix(c(25 * 2, 28 * 2), nrow = 1, ncol = 2)
  )
}

