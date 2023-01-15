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

# Set heigth plots
h_plots <- 700

## Header ---------------------------------------------------------------
header <- dashboardHeader(
  title = span(img(src = "diveRpine_v1.svg", height = '50'), "diveRpine v1.1.1"),
  tags$li(
    a(
      strong("About diveRpine"),
      height = 35,
      href = "https://github.com/ajpelu/diveRpine",
      title = "",
      targer = "_blank"
    ),
    class = "dropdown"
  )
)

## Sidebar ---------------------------------------------------------------
sidebar <- dashboardSidebar(disable = TRUE)

## Body ---------------------------------------------------------------
body <- dashboardBody(

  # Intro
  useShinyjs(),
  introjsUI(),

  fluidRow(
    column(width = 5,
           fluidRow(
             setSliderColor(c(rep("#5DB85C", 7)), c(1:7)), # change color sliders

             box(
               introBox(
                 tags$p(h4(strong("Pine plantation"))),
                 sliderInput( # Size of pine plantation
                   inputId = "pp_size", label = "Patch Area (ha)",
                   min = 200, max = 1500, value = 750, sep = ""
                 ),
                 awesomeRadio(
                   inputId = "pp_den", label = "Tree density",
                   choices = c("low", "medium", "high"),
                   selected = "medium", status = "success", inline = TRUE
                 ),
                 awesomeRadio(
                   inputId = "pp_use", label = "Past Land-use of the pine plantation",
                   choices = c("Natural Forests", "Pasture", "Shrublands", "Croplands"),
                   selected = "Shrublands", status = "success",
                   inline = TRUE
                 ),
                 tags$p(h5(strong("Climate Effect"))),
                 sliderInput( # Climate Effect - Elev
                   inputId = "elev", label = "Elevation",
                   min = 600, max = 2600, value = 1000,
                   sep = "", step = 25
                 ),
                 sliderInput( # Climate Effect - Rad
                   inputId = "rad",
                   label = HTML(paste0("Annual Radiation (GJ/m", tags$sup("2"), ")")),
                   min = 3, max = 7, value = 5, sep =".", step = 0.1
                 ),
                 data.step = 1,
                 data.intro = "Configure the stand features of the focal pine plantation: <ul> <li>Set the <b>patch size</b></li> <li>Specify the <b>tree density</b> </li> <li>Select the <b>past land-use</b></li> <li>Set the <b>elevation</b> and <b>radiation</b></li> </ul>"
               ),

               introBox(
                 tags$p(h4(strong("Natural Forests"))), # Natural forest configuration
                 sliderInput(
                   inputId = "nf_n", label = "Natural forests patch number",
                   min = 1, max = 5, value = 2
                 ),
                 sliderInput(
                   inputId = "nf_size",
                   label = "Natural forests patch size (min, max)",

                   # label = HTML(paste0("Natural forests patch size", br(), "(min and max)")),
                   min = 50, max = 500, value = c(100, 200)
                 ),
                 data.step = 2,
                 data.intro = "Configure the <b>landscape</b>. Specify the number and size of <b>natural forests</b>"
               ),

               introBox(
                 tags$br(),
                 actionBttn(
                   inputId = "createLandscape",
                   label = "Create Landscape",
                   color = "success",
                   style = "material-flat",
                   size = "xs"
                 ),
                 data.step = 3,
                 data.intro = "Click the button <b>Create Landscape</b> to plot the configurated landscape."
               ),

               tags$br(),
               tags$br(),

               introBox(
                 actionBttn(
                   inputId = "computeInitialRichness",
                   label = "Compute Initial Richness",
                   color = "success",
                   style = "material-flat",
                   size = "xs"
                 ),
                 data.step = 4,
                 data.intro = "Click the button <b>Compute Initial Richness</b> to calculate the initial composition of the focal pine-stand and natural forests patches. <br> The results can be seen in the output map (rigth-side) and a summary in the boxes below"
               )),

             box(
               introBox(
                 tags$p(h4(strong("Dispersers"))),
                 sliderInput(inputId = "sb",label = "Small-size Birds",
                             min = 0, max = 100, value = 0, step = 1),
                 uiOutput("mb"),
                 dataTableOutput("disptable"),
                 data.step = 5,
                 data.intro = "Configure the composition of the <b>dispersers community</b>. Select the percentage of medium and small-sized birds. The remaining will be assigned to the mammals category"
               ),

               tags$br(),
               tags$br(),
               introBox(
                 tags$p(h4(strong("Simulation"))),
                 sliderInput(inputId = "timeRange", label = "Simulation years:", min=10, max=50, value=30),
                 data.step = 7,
                 data.intro = "Specify the simulation time"),


               introBox(
                 actionBttn(inputId = "createPropagule", label = "Input seed propagules",
                            color = "success", style = "material-flat", size = "xs"),
                 data.step = 6,
                 data.intro = "Click the button to compute the <b>input seed propagules</b> into the focal pine-plantation"),

               tags$br(),
               tags$br(),
               introBox(
                 actionBttn(inputId = "computeEndRichness", label = "Compute Final Richness",
                            color = "success", style = "material-flat", size = "xs"),
                 data.step = 8,
                 data.intro = "Click the button to compute the <b> Final Richness</b> of the focal pine-stand. <br> The results can be seen in the output map (rigth-side) and a summary in the boxes below."
               )
             )
           ),

           fluidRow(
             box(
               width = 12,
               tags$p(h4(strong("Plant richness values by forest type"))),
               infoBoxOutput("rich_ppInitBox"),
               infoBoxOutput("rich_nfBox"),
               infoBoxOutput("rich_ppEndBox")
             )
           )
    ),
    column(width = 7,
           fluidRow(
             box(width = NULL,
                 uiOutput('plotMaps')
             ))
    )
  )
)

dashboardPage(header, sidebar, body,
              title = "diveRpine v1.1.0",
              skin = "green")
