library(shiny)
library(gridExtra)
library(shinycssloaders)
library(dplyr)



desc<-"Estimating the case fatality rate (CFR) of an ongoing pandemics is a complex problem. <br/> On average a person die of Covid19 after two weeks from the contagion, but delay in reporting the number of positives or deaths may alter this correlation. <br/> This app infers the lag between the trend of people positives to Covid19 and the deaths in a particular moment. <br/> A shorter lag can indicate a inefficient testing, while a longer one can indicate delay in reporting the deaths. <br/> This lag is then used to estimate the CFR for forecasting the number of future fatalities from the number of positives. <br/> You can select 3 different datasets offered by: the John Hopkins resource centre, the European Commission Joint Research Centre (JRC) and the Italian Protezione Civile for the Italian regions. <br/> After selecting the dataset you can choose a country / region."

# Define UI ----
ui <- fluidPage(
  titlePanel("PanDyCE - CFR estimator for Covid19"),

 sidebarLayout( 
  sidebarPanel(
    selectInput("source_data", h3("Select dataset"), 
                       choices = list("John Hopkins" = "JH", "John Hopkins (USA)" = "JHUSA", "JRC" = "JRC", "Protezione Civile (Italy)" = "PC"), selected = "JH"), 
    uiOutput("ui"),
 
    sliderInput("start_time", h3("Interval to scan"),
                       min = 1, max = 60, value = 45),
    sliderInput("time_window", h3("Moving window"),
                       min = 1, max = 120, value = 60),
    numericInput("force_ylim", 
                        h3("Max Y value"), 
                        value = 0),
    sliderInput("force_del", h3("Force lag"),
                       min = 0, max = 60, value = 0),

    sliderInput("go_back", h3("Go back in time"),
                       min = 0, max = 300, value = 0),

    sliderInput("cfr_time", h3("Days for CFR estimation"),
                       min = 2, max = 60, value = 30),

    sliderInput("for_time", h3("Days for forecast"), min=1, max=30, value=7,
                       step=1)
    ),

    mainPanel(
    p("Author: Luca Cozzuto"),
      tabsetPanel(
        tabPanel("Trend", htmlOutput("view"), plotOutput("plot") %>% withSpinner(color="#0dc5c1")),
        tabPanel("Corr", plotOutput("plot2")),
        tabPanel("CFR", plotOutput("plot3")),
        tabPanel("History", plotOutput("plot4")),
        tabPanel("Info", HTML(desc))
      )

    	#plotOutput('plot')
    	#tableOutput("view2")
    )
  )
)
