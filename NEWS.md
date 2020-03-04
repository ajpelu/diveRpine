# NEWS

# 1.0.1 diveRpine
## General comments
- Change name to diveRpine (diversification of pine plantation)
- Improve documentation of all functioms
- Add namespace of several functions, e.g.: `as()` to `methods::as()`


### initRichness.R
- fix minor typo on code comments

### distper.R
- Language traslation of some code comements

### createLandscape.R

- remove code block commented

`code r
 ## test geometry
  # pol_pine <- rasterToPolygons(pp, fun=function(x){x==pp_value}, dissolve = TRUE)
  # centroids <- as.data.frame(getSpPPolygonsLabptSlots(pol_pine))
  # names(centroids) <- c('x', 'y')
  # coordinates(centroids) <- ~x+y
  # pun <- circles(centroids, d=round(sqrt(((size_pp-50))/pi)), lonlat = FALSE)
  #
  # pp <- crop(pp, pun@polygons)
`

 - improve code styling
 - improve code comments
 - change ratify factor levels

 - Improve function documentation

 # 1.0.0 respineDocencia
 ## General comments
 - Release app for academic purposes
