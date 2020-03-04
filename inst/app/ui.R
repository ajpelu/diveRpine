library('shiny')
library('shinythemes')
library('shinycssloaders')
library('shinydashboard')
library('shinyWidgets')
library('knitr')
library('markdown')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')

# Set heigth plots
h_plots <- 800

header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable=TRUE)

body <- dashboardBody(
  fluidRow(
    column(width = 5,
      fluidRow(

        setSliderColor(c(rep("#28b78d", 6)), c(1:6)),

        box(tags$p(h4(strong("Pine plantation"))),
          sliderInput(inputId = "size_pp", label = "Patch Area",
                      min = 200, max = 1500, value = 750),
          selectInput(inputId = "density_pp", label = "Tree density",
                      choices = c('low', 'medium', 'high'), selected = 'medium'),
          selectInput(inputId = "pp_pastUse", label = "Past Land Use",
                      choices = c('Natural Forests', 'Shrublands', 'Pasture', 'Croplands'), selected = 'Shrublands'),

          tags$p(h4(strong("Natural Forests"))),
          sliderInput(inputId = "n_nf",label = "Patch numbers", min = 1, max= 5, value =2),
          sliderInput(inputId = "size_nf", label = "Patch size", min = 50, max = 500, value = 250),

          actionBttn(
            inputId = "doPaisaje",
            label = "Create Landscape",
            color = "success",
            style = "material-flat",
            size = "xs"
          ),
          tags$br(),
          tags$br(),

          actionBttn(
            inputId = "doRiquezaInit",
            label = "Compute Initial Richness",
            color = "success",
            style = "material-flat",
            size = "xs"
          )),


        box(
          tags$p(h4(strong("Dispersers"))),
          sliderInput(inputId = "sb",label = "Small-size Birds",
                      min = 0, max = 100, value = 0, step = 1),
          uiOutput("mb"),
          tableOutput("disptable"),
          tags$p(h4(strong("Simulation"))),
          sliderInput("timeRange", "Simulation years:", min=10, max=50, value=30),


          actionBttn(
            inputId = "doPropagulo",
            label = "Input seed propagules",
            color = "success",
            style = "material-flat",
            size = "xs"
          ),

          tags$br(),
          tags$br(),

          actionBttn(
            inputId = "doRiquezaEnd",
            label = "Compute Final Richness",
            color = "success",
            style = "material-flat",
            size = "xs")
          )
      ),

      fluidRow(
        #box(width=4, "Initial Richness Pine"),
        #box(width=4, "Initial Richness  Natural Forest"),
        #box(width=4, "Final Richness Pine"),
        infoBoxOutput("rich_ppInitBox"),
        infoBoxOutput("rich_nfBox"),
        infoBoxOutput("rich_ppEndBox")
        )
      ),
    column(width = 7,
           box(width = NULL,
                 uiOutput('plotMaps'))
           )
    )
  )

dashboardPage(header, sidebar, body, skin = 'green')
