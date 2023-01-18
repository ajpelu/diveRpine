# NEWS

# diveRpine 1.2.0
## General comments 
* Add a new vignette with all the packages used to develop the diveRpine.


## Major changes
* Init parameters: 
  
  * init_params.R script was removed from the app. The parameters were located at different functions or files. 
  * The new function `create_landscape()` creates an empty landscape
  * old `nf_value` specified at init_params were set by default to 2 in the functions `dist2nf()`, `potential_dispersion()`. 
  * old `pp_value` specified at init_params were set by default to 1 in the functions `potential_dispersion()`, and `input_progagule()`. 
  * A block of code with some initialize parameters was added to `server.R` of the shinyapp.
  * `ri_range`, *i.e.* the minimum and maximum values of plant richness in each of the landscape class are incorporated in the function `initRichness()`. In the app the default values of this dataframe was used for the study area (Sierra Nevada, southern Spain). 

* The documentation of functions has been updated and improved. 

* A new module has been implemented in the app to incorporate the climate-proxy functions, using the equation from [Gómez-Aparicio et al. (2009)](https://doi.org/10.1890/08-1656.1)


## Minor improvements and bug fixes
* fix minor typo on code comments of the functions
* Evaluates and set default arguments in `dist2nf()`, `initRichness()`, `input_progagule()`, `potential_dispersion()`.
* Improve the vignette of supporting references (using a `.bib` file). 


# diveRpine 1.1.0 
## General comments 
* Remove functions `createLandscape()`, `disper()`, and `disper_time()`. 
* Put some functions as code snippets of the shiny app. 
* Include new functions: `plot_landscape()`, `plot_progagule()`; `plot_richness()`, `potential_dispersion()`
* Add error standards for the richness values of the pine plantation
* Include new computation code for dispersion module. See `potential_dispersion()`. 

# diveRpine 1.0.1 
## General comments

* Change name to diveRpine (diversification of pine plantation)
* Improve documentation of all functioms
* Add namespace of several functions, e.g.: `as()` to `methods::as()`

## Minor improvements and bug fixes

* fix minor typo on code comments in initRichness.R
* createLandscape.R:
  
  * remove code block commented
  * improve code styling
  * improve code comments
  * change ratify factor levels

* Improve function documentation

# diveRpine  1.0.0 
## General comments

* Release app for academic purposes (respineDocencia)
