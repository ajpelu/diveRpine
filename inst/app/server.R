library('shiny')
library('shinycssloaders')
library('shinyWidgets')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')


### -------------------------------
# Load functions
source('createLandscape.R', local=TRUE)
source('initRichness.R', local = TRUE)
source('dist2nf.R', local = TRUE)
source('disper.R', local=TRUE)
source('disper_time.R', local=TRUE)
source('init_param.R', local=TRUE)

### -------------------------------
# SERVER
shinyServer(
  function(input, output, session){

    ### ----------------------------------------------
    # Show introduction text (Intro modal)
    observeEvent("", {
      showModal(modalDialog(
        includeHTML("intro_text.html"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "intro", label = "Introduction Tour", icon = icon("info-circle"))
        )
      ))
    })

    observeEvent(input$intro,{
      removeModal()
    })

    # Show tour
    observeEvent(input$intro,
                 introjs(session,
                         options = list("nextLabel" = "Continue",
                                        "prevLabel" = "Previous",
                                        "doneLabel" = "Done"))
    )


    ### ----------------------------------------------
    # Density
    den_pp <- reactive ({
      list(
        den = switch(input$density_pp, 'low' = 100, 'medium' = 1250, 'high' = 3000),
        col = switch(input$density_pp,'low' = '#a1d99b', 'medium' = '#238b45','high' = '#00441b'))
      })

    # Create landscape
    landscapeInit <- reactive({
      createLandscape(r, size_pp = input$size_pp, size_nf = input$size_nf, n_nf = input$n_nf)
      })

    # Past Use
    pastUse <- reactive({
      switch(input$pp_pastUse, 'Natural Forests' = 'Oak', 'Shrublands' = 'Shrubland',
             'Pasture' = 'Pasture','Croplands' = 'Crop')
      })

    ### ----------------------------------------------
    # Dispersion table
    ## slider conditioned to small_bird slider (see ui.R)
    output$mb <- renderUI({
      sliderInput(inputId = "mb",
                  label = "Medium-sized Birds",
                  min = 0, max = 100 - input$sb, value = 0)
      })

    perma <- reactive({
      100-(input$sb + input$mb)
    })


    output$disptable <- renderTable({
      tabla <- cbind(disperser = c('Small Birds', 'Medium Birds', 'Mammals'),
                     percentage = c(input$sb, input$mb, perma()))
      tabla},
      hover = TRUE, spacing = 'xs', align = 'c', digits = 0)


    ### ----------------------------------------------
    ## Distance raster
    dist_raster <- reactive({
      dist2nf(landscapeInit(), nf_value = nf_value)
      })

    ## Compute initial Richnness
    rasterRich <- reactive({
      initRichness(r = landscapeInit(), draster = dist_raster(),
                   r_range = ri_range, treedensity = den_pp()$den,
                   pastUse = pastUse(), rescale = FALSE)
    })

    ## Get bouondary of pp
    limit_pp <- reactive({
      rasterToPolygons(landscapeInit(), fun=function(x){x==pp_value}, dissolve = TRUE)
    })

    ## extension of Landscape Init
    ext <- reactive({
      list(
        xmin = extent(landscapeInit())@xmin,
        xmax = extent(landscapeInit())@xmax,
        ymin = extent(landscapeInit())@ymin,
        ymax = extent(landscapeInit())@ymax)
    })


    ### ----------------------------------------------
    ## Compute dispersion rasters
    rasterDisp <- reactive({
      disper(x = landscapeInit(), xr = rasterRich(), nf_value = nf_value, pp_value = pp_value)
      })

    ## Compute Richness pine plantations
    rich_pp <- reactive({
     calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
    })

    propagule_sb <- reactive({
      rasterDisp()[['msb']] * as.numeric(input$sb)
    })

    propagule_mb <- reactive({
      rasterDisp()[['mmb']] * as.numeric(input$mb)
    })

    propagule_ma <- reactive({
      rasterDisp()[['mma']] * as.numeric(perma())
    })


    propagule_bird_aux <- reactive({
      calc(stack(propagule_sb(), propagule_mb()),sum)
    })

    propagule_bird <- reactive({
      propagule_bird_aux() * piBird
    })

    propagule_mammal <- reactive({
      propagule_ma() * piMammal
    })

    propagule <- reactive({
      calc(stack(propagule_bird(), propagule_mammal()), sum)
      })

    ## Richness nf
    rich_nf <- reactive({
      rich_nf <- calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
      })

    ## Richness End
    rich_end <- reactive({
      propagulo_time <- rich_pp() + propagule()*input$timeRange

      rich_time <- calc(stack(landscapeInit(),
                              rasterRich(),
                              propagulo_time),
                        fun = function(x) ifelse(x[1] == pp_value, x[1]*x[3], x[2]))
      rich_time[rich_time== 0] <- NA

      list(
        rich_pp_end = propagulo_time,
        rich_time = rich_time)

    })


    ### ----------------------------------------------
    # Endpoints

    observeEvent(input$doPaisaje, {
      output$plotMaps <- renderUI({
        introBox(data.step = 4, data.intro = "Map",

        withSpinner(
          plotOutput("initial_map", height = h_plots),
        type=spinnerType, size=spinnerSize))
      })

      output$initial_map <- renderPlot({

        colores <- c('white', # Crops
                     den_pp()$col, # Pine plantation
                     'green', # Natural forests
                     'lightgoldenrod1') # Other

        key_landuses <- list(text = list(lab = c("Shrublands", "Pine plantation","Natural Forests","Croplands")),
                             rectangles=list(col = colores), space='bottom', columns=4)

            levelplot(landscapeInit(), att='landuse', scales=list(draw=FALSE),
                      col.regions = colores, colorkey=FALSE, key = key_landuses,
                      par.settings = list(axis.line = list(col = "transparent"),
                                          layout.heights = list(xlab.key.padding= 12)),
                      main = list("Initial Landscape configuration", cex=2)) +
              spplot(limit_pp(), fill = "transparent", col = "black",
                     xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                     colorkey = FALSE, lwd=line_pol)
      })

    })

    observeEvent(input$doRiquezaInit, {
      output$plotMaps <- renderUI({
        withSpinner(
        plotOutput("richness_map", height = h_plots),
        type=spinnerType, size=spinnerSize)})

      output$richness_map <- renderPlot({

          mapa_riqueza <- rasterRich()
          mapa_riqueza[mapa_riqueza == 0] <- NA

          levelplot(mapa_riqueza, par.settings = richness_theme, margin = FALSE,
                    scales=list(draw=FALSE), pretty=TRUE,
                    colorkey = list(space = "bottom"),
                    main=list('Initial Richness', cex = 2)) +
            spplot(limit_pp(), fill = "transparent", col = "black",
                   xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                   colorkey = FALSE, lwd=line_pol)
      })

    })

    observeEvent(input$doPropagulo, {
      output$plotMaps <- renderUI({
        withSpinner(
          plotOutput("richness_disper", height = h_plots),
          type=spinnerType, size=spinnerSize)})

      output$richness_disper <- renderPlot({
        levelplot(propagule(),
                  margin=FALSE,  par.settings = propagule_theme,
                  scales=list(draw=FALSE), colorkey = list(space = "bottom"),
                  main = list("Input propagule (n seed / (m^2 year))", cex=2))
      })
    })

    observeEvent(input$doRiquezaEnd, {
      output$plotMaps <- renderUI({
        withSpinner(
          plotOutput("richness_disperTime", height = h_plots),
          type=spinnerType, size=spinnerSize)})

      output$richness_disperTime <- renderPlot({
        rend <- rich_end()$rich_time
        levelplot(stack(rend),
                  par.settings = richness_theme, margin = FALSE, pretty=TRUE,
                  scales=list(draw=FALSE), colorkey = list(space = "bottom"),
                  main = list("Final richness", cex=2)) +
          spplot(limit_pp(), fill = "transparent", col = "black",
                 xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                 colorkey = FALSE, lwd=line_pol)

      })

    })


    ### ----------------------------------------------
    ## Richness Info Boxes
    output$rich_ppInitBox <- renderValueBox({
      valueBox(
               value = round(cellStats(rich_pp(), mean),2),
               subtitle =
                 HTML(paste0(
                   paste0(
                     round(cellStats(rich_pp(), min),2), " - ",
                     round(cellStats(rich_pp(), max),2)),
                   br(), "Initial Richness Pine")),
               icon = icon('tree-conifer', lib='glyphicon'), color = 'green')
    })

    output$rich_nfBox <- renderValueBox({
      valueBox(value = round(cellStats(rich_nf(), mean),2),
               subtitle =
                 HTML(paste0(
                   paste0(round(cellStats(rich_nf(), min),2), " - ",
                          round(cellStats(rich_nf(), max),2)),
                   br(), "Natural Forest Richness")),
               icon = icon('tree-deciduous', lib='glyphicon'), color = 'yellow')
    })

    output$rich_ppEndBox <- renderValueBox({
      valueBox(value = round(cellStats(rich_end()$rich_pp_end, mean),2),
               subtitle =
                 HTML(paste0(
                   paste0(round(cellStats(rich_end()$rich_pp_end, min),2), " - ",
                          round(cellStats(rich_end()$rich_pp_end, max),2)),
                   br(), "Final Richness Pine")),
               icon = icon('tree-conifer', lib='glyphicon'), color = 'olive')
    })

}

)


