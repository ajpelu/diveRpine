library("DT")
library("landscapeR")
library("raster")
library("rasterVis")
library("RColorBrewer")
library('rgeos')
library('rintrojs')
library('knitr')
library('markdown')
library('sp')
library("sf")
library('shiny')
library('shinythemes')
library('shinycssloaders')
library('shinydashboard')
library('shinyjs')
library('shinyWidgets')
library("tidyverse")

### -------------------------------
# Load functions
source("dist2nf.R")
source("init_params.R")
source("initRichness.R")
source("input_propagule.R")
source("plot_propagule.R")
source("plot_landscape.R")
source("plot_richness.R")
source("potential_dispersion.R")
source("summaryRaster.R")

shinyServer(
  function(input, output, session) {

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

  # Configure landscape module  ------------------
  ## Pine target submodule
  ### density
  pp_denR <- reactive({
    list(
      den = switch(input$pp_den,
                   "low" = 100,
                   "medium" = 1250,
                   "high" = 3000),
      col = switch(input$pp_den,
                   "low" = "#a1d99b",
                   "medium" = "#238b45",
                   "high" = "#00441b")
    )
  })

  ### Past Use
  pastUse <- reactive({
    switch(input$pp_use, 'Natural Forests' = 'Oak', 'Shrublands' = 'Shrubland',
           'Pasture' = 'Pasture','Croplands' = 'Crop')
  })

  ### Create pine plantation patch
  pine <- reactive({
    landscapeR::makePatch(empty_landscape,
                          val = 1, rast = TRUE, bgr = 0,
                          size = input$pp_size,
                          spt = position_pine)
  })

  ## Natural forests submodule
  ### Get the positions for the creation of the NF patches.
  nf_n <- reactive(input$nf_n)

  ### Get the positions for the creation of the NF patches.
  positions_nf <- reactive({
    sample(
      which(
        t(raster::as.matrix(pine())) == 0),
      nf_n()
    )
  })

  ### Generate the sizes of the natural forests patch
  nf_sizes <- reactive({
    round(runif(nf_n(),
                input$nf_size[1],
                input$nf_size[2]),
          digits = 0)
  })

  ## Generate pine + oak landscape
  pine_oak <- reactive({
    landscapeR::makeClass(pine(),
                          val = 2, rast = TRUE,
                          npatch = nf_n(),
                          pts = positions_nf(),
                          size = nf_sizes()
    )
  })

  ## Generate initial landscape
  crops_size <- reactive({
    sample(10:ceiling(
      length(which(t(raster::as.matrix(pine_oak())) == 0))*0.05),
      size = n_crops)
  })

  landscape <- reactive({
    landscapeR::makeClass(pine_oak(),
                          val = 3, rast = TRUE,
                          npatch = n_crops,
                          size = crops_size())
  })

  ## Get boundary of pp
  limit_pp <- reactive({
    limit_pp <- raster::rasterToPolygons(landscape(), fun=function(x){x==pp_value}, dissolve = TRUE)
    ggplot2::fortify(limit_pp, region = "layer") %>%
      dplyr::rename(x=long, y=lat)
  })

  # Compute initial Richness module ------------------

  ## Distance raster
  dist_raster <- reactive({
    dist2nf(landscape(), nf_value = nf_value) # nf defined at init_params
  })

  ## Initial richness
  rasterRich <- reactive({
    initRichness(r = landscape(), draster = dist_raster(),
                 r_range = ri_range, treedensity = pp_denR()$den,
                 pastUse = pastUse(), rescale = FALSE)
  })

  ## Richness of Natural forests
  rich_nf <- reactive({
    raster::calc(stack(landscape(), rasterRich()),
                 fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
  })

  ### Stats for nf richness
  rich_nfStats <- reactive({
    summaryRaster(rich_nf())
  })

  ## Initial Richness of Pine plantations
  rich_pp  <- reactive({
    raster::calc(stack(landscape(), rasterRich()),
                 fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
  })

  ### Stats for pp (init) richness
  rich_ppStats <- reactive({
    summaryRaster(rich_pp())
  })


  # Dispersion module ------------------

  ## Configure dispersion community. Compute disperser mammals
  perma <- reactive({
    100-(input$sb + input$mb)
  })

  ## Compute potential dispersion raster
  pot_disp <- reactive({
    potd <- potential_dispersion(x = landscape(), rich_nf = rich_nf(),
                                 nf_value = nf_value, pp_value = pp_value)
  })

  ## Compute the potential dispersion input for the pine-plantation
  pot_disp_pp <- reactive({
    input_propagule(x = landscape(), pd = pot_disp(), pp_value = pp_value)
  })


  ### contribution of each dispersers
  propagule <- reactive({
    propagule_sb <- pot_disp_pp()[['sb']] * as.numeric(input$sb) * piBird
    propagule_mb <- pot_disp_pp()[['mb']] * as.numeric(input$mb) * piBird
    propagule_ma <- pot_disp_pp()[['ma']] * as.numeric(perma()) * piMammal

    raster::calc(stack(propagule_sb,
                       propagule_mb,
                       propagule_ma), sum)
  })

  # Simulate module ------------------
  ## Compute the input over time
  rich_pp_end <- reactive({
    rich_pp() + propagule()*input$timeRange
  })


  rich_end <- reactive({
    raster::calc(stack(landscape(), rasterRich(), rich_pp_end()),
                 fun = function(x) ifelse(x[1] == pp_value, x[1]*x[3], x[2]))
    # rich_all_end[rich_all_end== 0] <- NA
  })

  ## Compute End Richness pine plantations stats
  rich_ppStats_end <- reactive({
    summaryRaster(rich_pp_end())
  })

  # ----------------------------------------------
  # ----------------------------------------------
  # Output: Plot Initial Landscape  ----------------------------------------------
  observeEvent(input$createLandscape, {
    output$plotMaps <- renderUI({
      plotOutput("initial_landscape", height = h_plots)})

    output$initial_landscape <- renderPlot({

      #limites_pp <- fortify(limit_pp(), region = "layer") %>% rename(x=long, y=lat)

      plot_landscape(landscape()) +
        ggplot2::scale_fill_manual(
          values =
            c("0" = "#FFFFe5",
              "1" = pp_denR()$col,
              "2" = "green",
              "3" = "lightgoldenrod1"),
          labels = c("Other", "Pine plantation", "Natural Forests", "Croplands"),
          name = "Present land uses"
        ) +
        ggplot2::geom_polygon(data=limit_pp(),
                              aes(x, y, group=group), fill=NA, colour="black", lwd=.8) +
        ggplot2::ggtitle("Initial Landscape configuration") +
        ggplot2::theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
                       legend.text = element_text(size = 16),
                       legend.title = element_text(size = 16))
    })
  })

  # Output: Plot Initial Richness  -----------------------
  observeEvent(input$computeInitialRichness, {
    output$plotMaps <- renderUI({
      plotOutput("richness_map", height = h_plots)})

    output$richness_map <- renderPlot({
      plot_richness(rasterRich()) +
        ggplot2::geom_polygon(data=limit_pp(),
                              aes(x, y, group=group), fill=NA, colour="black", lwd=.8) +
        ggplot2::ggtitle("Initial Richness") +
        ggplot2::theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
                       legend.text = element_text(size = 16),
                       legend.title = element_text(size = 16)) +
        ggplot2::labs(fill = " Nº plant species")
    })
  })

  # Output: Plot Propagule input --------------------------
  observeEvent(input$createPropagule, {
    output$plotMaps <- renderUI({
      plotOutput("seed_input", height = h_plots)
    })
    output$seed_input <- renderPlot({
      plot_propagule(propagule()) +
        ggplot2::ggtitle(
          expression("Input propagule (n seed" ~ m^-2 ~ year^-1*")")) +
        theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
              legend.text = element_text(size = 16))
    })
  })

  # Output: Plot End Richness ------------------------
  observeEvent(input$computeEndRichness, {
    output$plotMaps <- renderUI({
      plotOutput("richness_map_end", height = h_plots)})

    output$richness_map_end <- renderPlot({
      plot_richness(rich_end()) +
        ggplot2::geom_polygon(data=limit_pp(),
                              aes(x, y, group=group), fill=NA, colour="black", lwd=.8) +
        ggplot2::ggtitle("End Richness") +
        ggplot2::theme(plot.title = element_text(size = 24, face = "bold", hjust= 0.5),
                       legend.text = element_text(size = 16),
                       legend.title = element_text(size = 16)) +
        ggplot2::labs(fill = " Nº plant species")
    })
  })
  ### ----------------------------------------------

  # Output: Summary Richness Value boxes -------------
  ## Initial pine plantation
  output$rich_ppInitBox <- renderValueBox({
    valueBox(value =
               tags$p(HTML(paste0(
                 paste0(rich_ppStats()$mean, " &plusmn ", rich_ppStats()$sd))),
                 style = "font-size: 70%;"),
             subtitle =
               HTML(paste0(
                 paste0(rich_ppStats()$min, " - ", rich_ppStats()$max),
                 br(), tags$strong("Initial Pine plantation"))),
             icon = icon('tree-conifer', lib='glyphicon'), color = 'green')
  })

  ## Natural forests
  output$rich_nfBox <- renderValueBox({
    valueBox(value =
               tags$p(HTML(paste0(
                 paste0(rich_nfStats()$mean, " &plusmn ", rich_nfStats()$sd))),
                 style = "font-size: 70%;"),
             subtitle =
               HTML(paste0(
                 paste0(rich_nfStats()$min, " - ", rich_nfStats()$max),
                 br(), tags$strong("Natural Forest"))),
             icon = icon('tree-deciduous', lib='glyphicon'), color = 'yellow')
  })

  ## End pine plantation
  output$rich_ppEndBox <- renderValueBox({
    valueBox(value =
               tags$p(HTML(paste0(
                 paste0(rich_ppStats_end()$mean, " &plusmn ", rich_ppStats_end()$sd))),
                 style = "font-size: 70%;"),
             subtitle =
               HTML(paste0(
                 paste0(rich_ppStats_end()$min, " - ", rich_ppStats_end()$max),
                 br(), tags$strong("Final Pine plantation"))),
             icon = icon('tree-conifer', lib='glyphicon'), color = 'green')
  })

  # ----------------------------------------------
  # Output: Dispersers table  ---------
  output$mb <- renderUI({
    tagList(
      sliderInput(inputId = "mb",
                  label = "Medium-size birds",
                  min = 0, max = 100 - input$sb, value = 0)
    )
  })

  output$disptable <- DT::renderDataTable({
    name_disperser <- c("Small birds", "Medium birds", "Mammals")
    dispersers <- c(
      as.character(tags$img(src="smallbird.svg", height = '40', width = '50')),
      as.character(tags$img(src="garrulus.svg", height = '40', width = '50')),
      as.character(tags$img(src="vulpes.svg", height = '40', width = '50'))
    )
    percentage <- c(input$sb, input$mb, perma())

    DT::datatable(cbind(Dispersers = name_disperser,
                        icon = dispersers,
                        Percentage = percentage),
                  colnames = c("Dispersers", "", "%"),
                  escape = FALSE,
                  options = list(dom = 't'))
  })

  }
)
