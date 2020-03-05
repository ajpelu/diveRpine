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
library('rintrojs')
library('shinyjs')

# Set heigth plots
h_plots <- 800


sidebar <- dashboardSidebar(disable=TRUE)

# Header --------------------------------------------------------------------

header <- dashboardHeader(
  title = span(img(src = "diveRpine_v1.svg", height = 35), "diveRpine"),
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

# Body --------------------------------------------------------------------
body <- dashboardBody(

  # Intro
  useShinyjs(),
  introjsUI(),

  # Main body -------------------------------------------------------------
  fluidRow(
    column(width = 5,
      fluidRow(

        setSliderColor(c(rep("#28b78d", 6)), c(1:6)),

        box(
          introBox(
            tags$p(h4(strong("Pine plantation"))),
            sliderInput(inputId = "size_pp", label = "Patch Area",
                      min = 200, max = 1500, value = 750),
            selectInput(inputId = "density_pp", label = "Tree density",
                      choices = c('low', 'medium', 'high'), selected = 'medium'),
            selectInput(inputId = "pp_pastUse", label = "Past Land Use",
                      choices = c('Natural Forests', 'Shrublands', 'Pasture', 'Croplands'), selected = 'Shrublands'),
            data.step = 1,
            data.intro = "Configure the stand features of the focal pine plantation: <ul> <li>Set the <b>patch size</b></li> <li>Specify the <b>tree density</b> <li>Select the <b>past land-use</b></li> </ul>"
            ),
          introBox(
          tags$p(h4(strong("Natural Forests"))),
          sliderInput(inputId = "n_nf",label = "Patch numbers", min = 1, max= 5, value =2),
          sliderInput(inputId = "size_nf", label = "Patch size", min = 50, max = 500, value = 250),
          data.step = 2,
          data.intro = "Configure the <b>landscape</b>. Specify the number and size of <b>natural forests</b>"
          ),
          introBox(
          actionBttn(
            inputId = "doPaisaje",
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
            inputId = "doRiquezaInit",
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
          tableOutput("disptable"),
          data.step = 5,
          data.intro = "Configure the composition of the <b>dispersers community</b>. Select the percentage of medium and small-sized birds. The remaining will be assigned to the mammals category"),

          introBox(
          tags$p(h4(strong("Simulation"))),
          sliderInput("timeRange", "Simulation years:", min=10, max=50, value=30),
          data.step = 7,
          data.intro = "Specify the simulation time"),

          introBox(
            actionBttn(
            inputId = "doPropagulo",
            label = "Input seed propagules",
            color = "success",
            style = "material-flat",
            size = "xs"
          ),
          data.step = 6,
          data.intro = "Click the button to compute the <b>input seed propagules</b> into the focal pine-plantation"),

          tags$br(),
          tags$br(),

          introBox(
          actionBttn(
            inputId = "doRiquezaEnd",
            label = "Compute Final Richness",
            color = "success",
            style = "material-flat",
            size = "xs"),
          data.step = 8,
          data.intro = "Click the button to compute the <b> Final Richness</b> of the focal pine-stand. <br> The results can be seen in the output map (rigth-side) and a summary in the boxes below."
          )
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

dashboardPage(header, sidebar, body, skin = 'green',
              title = "diveRpine")
