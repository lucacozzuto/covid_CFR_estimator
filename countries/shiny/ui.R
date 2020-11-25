library(shiny)
library(gridExtra)

# Define UI ----
ui <- fluidPage(
  titlePanel("CFR estimator"),

 sidebarLayout( 
  sidebarPanel(
    uiOutput("ui"),
 
    selectInput("source_data", h3("Select dataset"), 
                       choices = list("John Hopkins" = "JH", "ECDC" = "ECDC",
                                      "Protezione Civile (Italy)" = "PC"), selected = "JH"), 
 
    sliderInput("start_time", h3("Interval to scan"),
                       min = 1, max = 60, value = 45),
    sliderInput("time_window", h3("Moving window"),
                       min = 1, max = 120, value = 90),
    numericInput("force_ylim", 
                        h3("Max Y value"), 
                        value = 0),
    sliderInput("force_del", h3("Manual delay"),
                       min = 0, max = 60, value = 0),

    sliderInput("go_back", h3("Go back in time"),
                       min = 0, max = 120, value = 0),

    sliderInput("for_time", h3("Time for forecast"),
                       min = 1, max = 30, value = 7),
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Trend", plotOutput("plot")),
        tabPanel("Var", plotOutput("plot2")),
        tabPanel("CFR", plotOutput("plot3"))
      )

    	#plotOutput('plot')
    	#tableOutput("view2")
    )
  )
)