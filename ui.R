# include script that reads all data, loads libraries, and does some preprocessing
source("setup.R")

ui <- navbarPage(
  title = "Data i Dialog",
  position = "fixed-top",
  tags$style(type = "text/css", "body {padding-top: 70px;}"),
  theme = bs_theme(
    version = 4,
    bootswatch = "sandstone",
    base_font = font_google("Lato")
  ),

# Stockholmsenkäten -------------------------------------------------------

  navbarMenu(
    "Stockholmsenkäten",

## Kommunöversikt ----------------------------------------------------------

    tabPanel(
      "Kommunöversikt",
      sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:25%;",
          # get inputs
          sliderInput("years0", label = "Årtal", max = 2022, min = 2006, step = 2, value = 2022, sep = ""),
          selectInput("enkommun", label = "Kommun", choices = kommuner, selected = "Vallentuna"),
          selectInput("factor", label = "Riskfaktor", choices = rfactors),
          selectInput("pfactor", label = "Skyddsfaktor", choices = pfactors, selected = "Positiv skolanknytning")
        ),
        mainPanel(
          plotOutput("circlePlot", height = 600), # , width = "100%"
          plotOutput("stackedArea"),
          br(),
          plotOutput("stackedAreaÅK"),
          br(),
          plotOutput("ProtAreaPlot"),
          br(),
          plotOutput("ProtAreaPlotÅK")
        )
      )
    ),

## Demografi ---------------------------------------------------------------
    
    tabPanel(
      "Demografi",
      sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:25%;",
          # get inputs
          
          selectInput("demogrVar",
                      label = "Demografiska variabler",
            choices = demogr.choices,
            selected = c("Hur länge har du bott i Sverige?")
          ),
          checkboxGroupInput("kommunDemo", label = "Kommun", choices = kommuner, 
                             selected = c("Vaxholm", "Vallentuna"))
          #sliderInput("yearsDemo", label = "Årtal", max = 2022, min = 2006, step = 2, 
          #            value = c(2018, 2022), sep = "")
        ),
        mainPanel(
          plotOutput("demogrF5", height = 500),
          br(),
          plotOutput("demogrAntal", height = 500)
        )
      )
    ),

## Jämförelser -------------------------------------------------------------

    tabPanel(
      "Jämförelser",
      sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:25%;",
          # get inputs
          selectInput("factor2", label = "Risk-/skyddsfaktor", choices = rsfaktorer, 
                      selected = "Psykiska/ psykosomatiska besvär"),
          selectInput("year2", label = "Årtal för distribution", choices = årtal, selected = "2020"),
          checkboxGroupInput("kommun", label = "Kommun", choices = kommuner,
                             selected = c("Vaxholm", "Vallentuna", "Danderyd", "Södertälje")),
          sliderInput("years2", label = "Årtal linjediagram", max = 2022, min = 2006, step = 2, 
                      value = c(2016, 2022), sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("radarPlot", width = 800),
          
          plotOutput("line90"),
          br(),
          plotOutput("lineYearsMean"),
          br(),
          plotOutput("slab2multi"),
          br(),
          plotOutput("circlePlotWrap", height = 700)
        )
      )
    ),
    tabPanel(
      "Exempel enkät",
        mainPanel(
          radioButtons("risknivå",
            label = "Välj risknivå för Psykiska/ psykosomatiska besvär.",
            choices = c("Låg risk", "Något förhöjd risk", "Förhöjd risk"), selected = "Låg risk",
            inline = T
          ),
          formattableOutput("personaTable", width = 800),
          plotOutput("personaPlot", width = 800)
        )
      ),
tabPanel(
  "Social gradient",
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:25%;",
      # get inputs
      selectInput("factor4", label = "Risk-/skyddsfaktor (Index)", choices = rsfaktorer),
      selectInput("socialg", label = "Demografisk variabel", choices = demogr.choices,
                  selected = "Vad bor du i för typ av bostad?"),
      sliderInput("years4", label = "Årtal", max = 2022, min = 2006, step = 2, 
                  value = c(2018, 2022), sep = ""),
      selectInput("enkommun4", label = "Kommun", choices = c("Vaxholm", "Vallentuna", "Danderyd", "Täby","Södertälje"),
                  selected = "Södertälje")
    ),
    mainPanel(
      #plotOutput("areaPlotSoc"),
      #br(),
      plotOutput("SocLineYearsMean")
    )
  )
)
  ),

# Skolinspektionen --------------------------------------------------------


  navbarMenu(
    "Skolinspektionen",
    tabPanel(
      "Jämförelser",
      sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:25%;",
          # get inputs
          selectInput("SIenkommun", label = "Kommun", 
                      choices = c("Vaxholm", "Vallentuna", "Stockholm")),
          #sliderInput("SIyears",
          #  label = "Årtal", max = 2021, min = 2015, step = 1,
          #  value = c(2016, 2021), sep = ""
          #),
          checkboxGroupInput("SIkommuner",
            label = "Kommun",
            choices = c("Vaxholm", "Vallentuna", "Stockholm"),
            selected = c("Vaxholm", "Vallentuna", "Stockholm")
          )
        ),
        mainPanel(
          plotOutput("SIareaPlot"),
          br(),
          plotOutput("SIyearsMean"),
          br(),
          #plotOutput("siLine90")
        )
      )
    ),
    tabPanel(
      "Demografi"
    )
  ),


# KOLADA ------------------------------------------------------------------

tabPanel( # change to tabsetPanel. we can then use one panel only to select variables
  "KOLADA",
  tabsetPanel(
    tabPanel(
      "Samtliga",
      plotOutput("kolaMpanelAll", height = 400),
      br()
    ),
    tabPanel(
      "Uppdelat på kön",
      plotOutput("kolaMpanelGender", height = 400),
      br()
    ),
    tabPanel(
      "Samtliga barplot",
      plotOutput("kolaMpanelAllBars", height = 400),
      br()
    )
    ),
    fluidRow(
      column(
        2,
        wellPanel(
          # style = "position:fixed;width:25%;",
          # get inputs
          checkboxGroupInput("kolaKommuner",
            label = "Kommun",
            choices = kommuner, selected = c("Vaxholm", "Vallentuna", "Södertälje", "Danderyd")
          )
        )
      ),
      column(
        8,
        wellPanel(
          checkboxGroupInput("kolaKPI",
            label = "Nyckeltal",
            choices = kpiChoices, selected = kpiChoices[c(1, 2, 4, 5)]
          )
        )
      )
    )
  ),

      
    


# RS-faktorer  --------------------------------------------------------


  tabPanel(
    "Risk- och skyddsfaktorer",
    uiOutput("rsFigure"),
    br(),
    fluidRow(
      column(
        3,
        wellPanel(
          style = "position:fixed;width:25%;",
          # get inputs
          selectInput("context", label = "Kontext", choices = rskontext, selected = "Skola"),
          selectInput("factortype", label = "Risk- eller skydd", choices = c("Riskfaktor", "Skyddsfaktor"))
        )
      )
    )
  ),

# Insatser --------------------------------------------------------


  tabPanel(
    "Insatser",
    gt_output("insatserTable")
  )
)