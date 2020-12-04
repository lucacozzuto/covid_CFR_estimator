#rsconnect::deployApp('./')
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library("EnvStats")
library("spatialEco")
library("berryFunctions")
library("zoo")
library(reshape)
library(gridExtra)
library(shinycssloaders)
#options(shiny.sanitize.errors = FALSE)


# get the data from John Hopkins
death_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
cases_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
death_web_US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
cases_web_US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"


ecdc_web <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
ita_web<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

getDataFromITA <- function(link) {
	data_raw<-read.csv(link)
	data_raw$"country"<-data_raw$denominazione_regione
	sel_data<-data_raw[, c("country", "data", "nuovi_positivi", "deceduti")]
	colnames(sel_data)<-c("country", "date", "cases", "deaths")
	sel_data$date<-as.Date(sel_data$date,)
	return (sel_data)
}

getDataFromECDC <- function(link) {
	data_raw<-read.csv(link,  na.strings = "", fileEncoding = "UTF-8-BOM")
	data_raw$country<-data_raw$countriesAndTerritories
	sel_data<-data_raw[, c("country", "dateRep", "cases", "deaths")]
	colnames(sel_data)<-c("country", "date", "cases", "deaths")
	sel_data$date<-as.Date(sel_data$date, format="%d/%m/%Y")
	return (sel_data)
}

getDataFromJH <- function(link, us=FALSE) {
	data_raw<-read.csv(link)
	# aggregate data and remove not useful ones
	if (us) {
		data_raw_sub<-subset(data_raw, select = -c(Country_Region,UID,iso2,iso3,code3,FIPS,Admin2,Lat,Long_,Combined_Key))
		if("Population" %in% colnames(data_raw_sub)) {
			data_raw_sub$Population<-NULL
		}
		colnames(data_raw_sub)[1] <- "Country.Region"
		data_raw<-data_raw_sub
	} else {
		data_raw$Province.State<-NULL
		data_raw$Lat<-NULL
		data_raw$Long<-NULL
	}
	data_raw.agg<-data_raw %>%
		group_by(Country.Region) %>% 
		summarize_all(sum, na.rm = TRUE)
	data_raw.agg.df<-as.data.frame(data_raw.agg)
	data_raw.agg.df$country<-data_raw.agg.df$Country.Region
	data_raw.agg.df$Country.Region<-NULL	
	mdata<-melt(data_raw.agg.df, id=c("country"))
	mdata$variable<-as.Date(str_replace_all(str_replace(mdata$variable, "X", ""), '\\.', "-"), format="%m-%d-%y")
	return(mdata)
}

getUSDataFromJH <- function(link) {
	data_raw<-read.csv(link)
	# aggregate data and remove not useful ones
	us_data<-data_raw[grep("US", data_raw$Country.Region), ]

	data_raw$Province.State<-NULL
	data_raw$Lat<-NULL
	data_raw$Long<-NULL
	data_raw.agg<-data_raw %>%
		group_by(Country.Region) %>% 
		summarize_all(sum, na.rm = TRUE)
	data_raw.agg.df<-as.data.frame(data_raw.agg)
	data_raw.agg.df$country<-data_raw.agg.df$Country.Region
	data_raw.agg.df$Country.Region<-NULL
	mdata<-melt(data_raw.agg.df, id=c("country"))
	mdata$variable<-as.Date(str_replace_all(str_replace(mdata$variable, "X", ""), '\\.', "-"), format="%m-%d-%y")
	return(mdata)
}



getSingleCountryData <- function(data_all, country, source ) {
	single_data<-data_all[grep(country, data_all$country), ]
	merge_data<-single_data
	if (source != "ECDC") {
		death<-as.data.frame(diff(single_data$deaths))
		row.names(death)<-tail(row.names(single_data), -1)
		merge_data<-merge(tail(single_data, -1), death, by="row.names")
		merge_data$Row.names<-NULL
		merge_data$deaths<-NULL
		names(merge_data)<-c("country", "date","cases", "deaths")
		merge_data <- merge_data[order(merge_data$date),]
		rownames(merge_data) <- 1:nrow(merge_data)
	} 
	if (grepl( "JH", source, fixed = TRUE)) {
		pos<-as.data.frame(diff(merge_data$cases))
		row.names(pos)<-tail(row.names(merge_data), -1)
		merge_data2 <- merge(tail(merge_data, -1), pos, by="row.names")
		merge_data2$Row.names<-NULL
		merge_data2$cases<-NULL
		names(merge_data2)<-c("country", "date","deaths", "cases")
		merge_data <- merge_data2[, c(1, 2, 4, 3)]
		merge_data<-merge_data2	
	}
	ord_data <- merge_data[order(merge_data$date),]
	rownames(ord_data) <- 1:nrow(ord_data)
	return (ord_data)
}

getFit <- function(data_to_fit, time_mavg) {
	require(pracma)
	fitted_data <- movavg(as.numeric(data_to_fit), time_mavg, type=c("s"))
	return(fitted_data)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

plotVAR<-function(dateshiftdiff, country, start_time, time_window, source_data, go_back=0, for_time=0, force_ylim=0, force_del=0, cfr_time=30) {
	if (as.numeric(go_back > 0)) {
		dateshiftdiff<-head(dateshiftdiff, -as.numeric(go_back))
	}
	last_day<-tail(dateshiftdiff$date, 1)

	time_mavg<-7
	# make moving average
	fitdT7 <- getFit(dateshiftdiff$deaths, time_mavg)
	fitT7 <- getFit(dateshiftdiff$cases, time_mavg)

	# max Y
	ylim_cases <- max(dateshiftdiff$cases)

	if (force_ylim > 0) {
		ylim_cases <- as.numeric(force_ylim)
	}
	ylim_deaths <- ylim_cases/10

	### predict delay
	props_t<-c()
	fc_t<-c()
	stdevcfr_t<-c()
	cvdevcfr_t<-c()

	for(i in 1:start_time) {
	    props_t <- tail(fitdT7, -i)/head(fitT7, -i)
    #	fc_t[i] <- mean(tail(props_t, n=time_window), trim = 0.10)
    #median absolute deviation or stdev without outliers?
    #stdevcfr_t[i] <- mad(tail(props_t, n=time_window))
		# remove outliers and calc average and stdev
		no_out<-remove_outliers(tail(props_t, n=cfr_time))
    	fc_t[i] <- mean(no_out, na.rm =TRUE)
		stdevcfr_t[i] <- sd(no_out, na.rm =TRUE)
    	win_pos<-tail(tail(fitdT7, -i), time_window)
    	win_deat<-tail(head(fitT7, -i), time_window)
		cvdevcfr_t[i] <- cor.test(win_pos, win_deat)$p.value
	}

	delay_time<-which.min(cvdevcfr_t)

	if (force_del > 0) {
		delay_time = as.numeric(force_del)
	}

	forecast_time<-delay_time
	
	if (as.numeric(for_time) == 0) {
   		forecast_time<-delay_time
	} else if (as.numeric(for_time) > delay_time) {
		forecast_time<-delay_time
	} else { forecast_time<-as.numeric(for_time) }


	#fname2<-paste("VAR_", country, "_", source_data, "_", format(last_day, "%d-%m-%y"),   ".png", sep="")
	#png(fname2)
	plotmin<-locmin<-local.min.max(log(cvdevcfr_t))
	minim_idx<-match(locmin$minima, log(cvdevcfr_t))
	title(main = paste0("Estimated delay is: ", delay_time), sub=paste0("Minima: ", paste(minim_idx, collapse=",")))	

}



plotTrend<-function(dateshiftdiff, country, start_time, time_window, source_data, go_back=0, for_time=0, force_ylim=0, force_del=0, cfr_time=30) {
	if (as.numeric(go_back > 0)) {
		dateshiftdiff<-head(dateshiftdiff, -as.numeric(go_back))
	}
	last_day<-tail(dateshiftdiff$date, 1)

	time_mavg<-7
	# make moving average
	fitdT7 <- getFit(dateshiftdiff$deaths, time_mavg)
	fitT7 <- getFit(dateshiftdiff$cases, time_mavg)

	# max Y
	ylim_cases <- max(dateshiftdiff$cases)

	if (force_ylim > 0) {
		ylim_cases <- as.numeric(force_ylim)
	}
	ylim_deaths <- ylim_cases/10


	### predict delay
	props_t<-c()
	fc_t<-c()
	stdevcfr_t<-c()
	cvdevcfr_t<-c()


	for(i in 1:start_time) {
	    props_t <- tail(fitdT7, -i)/head(fitT7, -i)
    #	fc_t[i] <- mean(tail(props_t, n=time_window), trim = 0.10)
    #median absolute deviation or stdev without outliers?
    #stdevcfr_t[i] <- mad(tail(props_t, n=time_window))
		# remove outliers and calc average and stdev
		no_out<-remove_outliers(tail(props_t, n=cfr_time))
    	fc_t[i] <- mean(no_out, na.rm =TRUE)
		stdevcfr_t[i] <- sd(no_out, na.rm =TRUE)
    	win_pos<-tail(tail(fitdT7, -i), time_window)
    	win_deat<-tail(head(fitT7, -i), time_window)
		cvdevcfr_t[i] <- cor.test(win_pos, win_deat)$p.value
	}

	delay_time<-which.min(cvdevcfr_t)

	if (force_del > 0) {
		delay_time = as.numeric(force_del)
	}

	forecast_time<-delay_time
	
	if (as.numeric(for_time) == 0) {
   		forecast_time<-delay_time
	} else if (as.numeric(for_time) > delay_time) {
		forecast_time<-delay_time
	} else { forecast_time<-as.numeric(for_time) }

	props<-tail(fitdT7, -delay_time)/head(fitT7, -delay_time)*100
	fc<-fc_t[delay_time]
	stdevcfr<-stdevcfr_t[delay_time]

	forecast<-sum(dateshiftdiff$deaths)

	for_min <- forecast
	for_max <- for_min
	minstd<-fc-(3*stdevcfr)
	maxstd<-fc+(3*stdevcfr)

	if (minstd < 0) {
		minstd<-0
	}
	
	for(i in 1:forecast_time) {
		forecast = forecast + fitT7[length(props)+i]*fc
		for_min = for_min + fitT7[length(props)+i]*(minstd)
		for_max = for_max + fitT7[length(props)+i]*(maxstd)
	}
	for_min = round(for_min)
	for_max = round(for_max)

	diff_for<-round(forecast)-sum(dateshiftdiff$deaths)
	forecast<-round(forecast, 0)
	par(mar=c(10, 8, 4, 10) + 0.1)

	perday<-format(round((forecast-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")
	perdaymin<-format(round((for_min-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")
	perdaymax<-format(round((for_max-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")

	subt1<-paste(country, format(sum(dateshiftdiff$deaths), big.mark=","),"deaths.", source_data, format(last_day, "%d-%m-%y"), "The delay is", delay_time, "days", "CFR is:", round(fc_t[delay_time]*100, 2), "+/-", 3*round(stdevcfr*100, 2), "%", sep=" ")
	subt2<-paste("Forecast in",forecast_time, "days", format(forecast, big.mark=","), "(", format(for_min, big.mark=","), "/", format(for_max, big.mark=","),").", "Per day:", perday, "(", perdaymin, "/", perdaymax, ")", sep=" ")
	plot(zoo((dateshiftdiff$cases), dateshiftdiff$date), xaxt='n', yaxt='n', main = paste(c(subt1, subt2), sep=""), ylim=c(0,ylim_cases), type = c("p"), cex=0.5, lty=0, pch=16, ylab="", xlab="", col ="blue") 
	timeAxis(1, midmonth=TRUE, format="%b")


	lines(zoo((fitT7), dateshiftdiff$date), col="blue")
	lines(zoo((fitdT7)*1/fc_t[delay_time], dateshiftdiff$date-delay_time), col="darkblue", lty=3)

	axis(2, col="blue",col.axis="blue",las=1)
	mtext("New cases",side=2,line=4, col="blue")

	box()

	par(new=TRUE)

	plot(zoo((dateshiftdiff$deaths), dateshiftdiff$date), yaxt='n', xaxt="n", type = c("p"), cex=0.5, lty=0, ylim=c(0,ylim_deaths), xlab="", pch=3, ylab="", col ="red") 
	lines(zoo((fitdT7), dateshiftdiff$date), col="red")
	mtext("New deaths",side=4,col="red",line=4) 
	axis(4, col="red", col.axis="red",las=1)
	
	par(new=TRUE)
	par(mar=c(0, 8, 35, 10) + 0.1)

	props[1:65]<-0
	barplot(-props, yaxt='n', ylim=c(-10,0), col=rgb(red=51, green=51, blue=255, alpha=10, maxColorValue=255))

	axis(2, cex.axis=.7, labels=FALSE, at=c(0,-2,-4,-8) ,tck = 0.02, font=1)
	text(-1, c(-1, -3,-5,-9), c("0%","2%","4%","8%"), cex = 1)
	abline(h=-2, col="black", lty=2)
	mtext("Proportion",side=2,col="black",line=2) 

}

plotCFR<-function(dateshiftdiff, country, start_time, time_window, source_data, go_back=0, for_time=0, force_ylim=0, force_del=0, cfr_time=30) {
	if (as.numeric(go_back > 0)) {
		dateshiftdiff<-head(dateshiftdiff, -as.numeric(go_back))
	}
	last_day<-tail(dateshiftdiff$date, 1)

	time_mavg<-7
	# make moving average
	fitdT7 <- getFit(dateshiftdiff$deaths, time_mavg)
	fitT7 <- getFit(dateshiftdiff$cases, time_mavg)

	# max Y
	ylim_cases <- max(dateshiftdiff$cases)

	if (force_ylim > 0) {
		ylim_cases <- as.numeric(force_ylim)
	}
	ylim_deaths <- ylim_cases/10

	### predict delay
	props_t<-c()
	fc_t<-c()
	stdevcfr_t<-c()
	cvdevcfr_t<-c()

	for(i in 1:start_time) {
	    props_t <- tail(fitdT7, -i)/head(fitT7, -i)
    #	fc_t[i] <- mean(tail(props_t, n=time_window), trim = 0.10)
    #median absolute deviation or stdev without outliers?
    #stdevcfr_t[i] <- mad(tail(props_t, n=time_window))
		# remove outliers and calc average and stdev
		no_out<-remove_outliers(tail(props_t, n=cfr_time))
    	fc_t[i] <- mean(no_out, na.rm =TRUE)
		stdevcfr_t[i] <- sd(no_out, na.rm =TRUE)
    	win_pos<-tail(tail(fitdT7, -i), time_window)
    	win_deat<-tail(head(fitT7, -i), time_window)
		cvdevcfr_t[i] <- cor.test(win_pos, win_deat)$p.value
	}
	
	delay_time<-which.min(cvdevcfr_t)

	if (force_del > 0) {
		delay_time = as.numeric(force_del)
	}

	forecast_time<-delay_time
	
	if (as.numeric(for_time) == 0) {
   		forecast_time<-delay_time
	} else if (as.numeric(for_time) > delay_time) {
		forecast_time<-delay_time
	} else { forecast_time<-as.numeric(for_time) }

	props<-tail(fitdT7, -delay_time)/head(fitT7, -delay_time)*100
	fc<-fc_t[delay_time]
	stdevcfr<-stdevcfr_t[delay_time]

	forecast<-sum(dateshiftdiff$deaths)

	for_min <- forecast
	for_max <- for_min
	minstd<-fc-(3*stdevcfr)
	maxstd<-fc+(3*stdevcfr)

	if (minstd < 0) {
		minstd<-0
	}
	
	for(i in 1:forecast_time) {
		forecast = forecast + fitT7[length(props)+i]*fc
		for_min = for_min + fitT7[length(props)+i]*(minstd)
		for_max = for_max + fitT7[length(props)+i]*(maxstd)
	}
	for_min = round(for_min)
	for_max = round(for_max)

	diff_for<-round(forecast)-sum(dateshiftdiff$deaths)
	forecast<-round(forecast, 0)


	cfr<-(tail(fitdT7, -delay_time)/head(fitT7, -delay_time))
	fname<-paste("CFR_", country, "_", source_data, "_", format(last_day, "%d-%m-%y"), ".png", sep="")

	cfr_perc<-cfr*100

	plot(zoo(cfr_perc, tail(dateshiftdiff$date, -delay_time)), xaxt='n',yaxs="i" , type = c("l"), ylim=c(0,10), xlab="Months", ylab="CFR (in %).", col ="red") 
	timeAxis(1, midmonth=TRUE, format="%b")
	abline(h=1, col="gray")
	abline(h=2, col="gray")

}







deat_jh<-getDataFromJH(death_web)
pos_jh<-getDataFromJH(cases_web)
colnames(deat_jh)<-c("country", "date", "deaths")
colnames(pos_jh)<-c("country", "date", "cases")
jh_data<-pos_jh
jh_data$deaths<-deat_jh$deaths

deat_jh_us<-getDataFromJH(death_web_US, TRUE)
pos_jh_us<-getDataFromJH(cases_web_US, TRUE)
colnames(deat_jh_us)<-c("country", "date", "deaths")
colnames(pos_jh_us)<-c("country", "date", "cases")
jhus_data<-pos_jh_us
jhus_data$deaths<-deat_jh_us$deaths


ecdc_data<-(getDataFromECDC(ecdc_web))
ita_data<-(getDataFromITA(ita_web))

#single_country_data<-getSingleCountryData(jhus_data, "Alabama", "JHUS")


# Define server logic ----
server <- function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$source_data,
           "ECDC" = ecdc_data,
           "JH" = jh_data,
           "JHUSA" = jhus_data,
           "PC" = ita_data)
  })
   # choose the country
   output$ui <- renderUI({
		source_data<-datasetInput()
  		countries <- unique(source_data$country)
    	selectInput("country", h3("Choose country"), countries, selected="Italy") 
	})

   output$ui_cfr <- renderUI({
    sliderInput("cfr_time", h3("Days for CFR estimation"),
                       min = 2, max = input$time_window, value = 30)
	})

	output$plot <- renderPlot({	
		#validate(
		#	need(is.null(datasetInput()), ""),
		#	need(is.null(input$country), "")
		#)
		single_country_data<-getSingleCountryData(datasetInput(), input$country, input$source_data)      
    	plotTrend(single_country_data, input$country, input$start_time , input$time_window, input$source_data,  input$go_back, input$for_time, input$force_ylim, input$force_del , input$cfr_time)
    }, 	height=600, width=1200)

	output$plot2 <- renderPlot({
		single_country_data<-getSingleCountryData(datasetInput(), input$country, input$source_data)
 		plotVAR(single_country_data, input$country, input$start_time , input$time_window, input$source_data,  input$go_back, input$for_time, input$force_ylim, input$force_del , input$cfr_time)
    }, height=600, width=600)

	output$plot3 <- renderPlot({
		single_country_data<-getSingleCountryData(datasetInput(), input$country, input$source_data)
 		plotCFR(single_country_data, input$country, input$start_time , input$time_window, input$source_data,  input$go_back, input$for_time, input$force_ylim, input$force_del , input$cfr_time)
    }, height=600, width=600)
       		
  output$view <- renderTable({
    tail(getSingleCountryData(datasetInput(), input$country, input$source_data), n = 10)
  })
}