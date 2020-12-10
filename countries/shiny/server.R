#rsconnect::deployApp('./')
library(shiny)
library(shinycssloaders)
source("functions.R")

# Get the data
# JH
jh_data<-getDataFromJH(death_web, cases_web)
# JHUSA
jhus_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
# ECDC
ecdc_data<-(getDataFromECDC(ecdc_web))
# PC
ita_data<-(getDataFromITA(ita_web))

#Example 
#single_country_data<-getSingleCountryData(jhus_data, "Alabama", "JHUSA")
#predCFR<-calcCFR(single_country_data, input$start_time, input$time_window, input$go_back,input$for_time, input$force_del, input$cfr_time)
#predCFR<-calcCFR(single_country_data, 45, 90, NULL, 7, NULL, 30)


# Define server logic ----
server <- function(input, output, session) {
 
  # Return the requested dataset
  datasetInput <- reactive({
    req(input$source_data)
    switch(input$source_data,
           "ECDC" = ecdc_data,
           "JH" = jh_data,
           "JHUSA" = jhus_data,
           "PC" = ita_data)
  })

    observe({
    
       # choose the country
   output$ui <- renderUI({
		source_data<-datasetInput()
  		countries <- unique(source_data$country)
    	selectInput("country", h3("Choose country"), countries, selected="Italy") 
	})

  plot = reactiveVal()
  getData = reactiveVal()
 	      
 	getData = reactive({
		res = NULL
	  	single_country_data = getSingleCountryData(datasetInput(), input$country, input$source_data)
		if (!is.null(single_country_data)) {
			res =calcCFR(single_country_data, input$start_time, input$time_window, input$go_back, input$for_time, input$force_del, input$cfr_time  )
		} 
		return(res)
	})
	
	output$plot <- renderPlot({	
		if (!is.null(getData())) {
			plotTrend(getData(), input$force_ylim)
		}
    }, 	height=600, width=1200)

	output$plot2 <- renderPlot({
		if (!is.null(getData())) {
			plotVAR(getData())
    	}
    }, height=600, width=600)

	output$plot3 <- renderPlot({
		if (!is.null(getData())) {
			plotCFR(getData())
		}
    }, height=600, width=600)
       		
  output$view <- renderText({
	if (!is.null(getData())) {
		makeRes(getData(), input$source_data, input$for_time)
  	} else {
  		paste0("Loading data...")
  	}
  })
  
  })
}