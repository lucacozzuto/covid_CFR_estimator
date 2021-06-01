#rsconnect::deployApp('./')
library(ggplot2)
library(dplyr)
library(stringr)
library("EnvStats")
library("spatialEco")
library("berryFunctions")
library("zoo")
library(reshape)
library(gridExtra)
#libray(apricom)
library(cowplot)
library(scales)

			
# get the data from John Hopkins
death_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
cases_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
death_web_US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
cases_web_US <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jrc_web <- "https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv"
ita_web<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"


getDataFromITA <- function(link) {
	data_raw<-read.csv(link)
	data_raw$"country"<-data_raw$denominazione_regione
	sel_data<-data_raw[, c("country", "data", "nuovi_positivi", "deceduti")]
	colnames(sel_data)<-c("country", "date", "cases", "deaths")
	sel_data$date<-as.Date(sel_data$date,)
	return (sel_data)
}

getDataFromJRC <- function(link) {
	data_raw<-read.csv(link,  na.strings = "", fileEncoding = "UTF-8-BOM")
	data_raw$country<-data_raw$countriesAndTerritories
	sel_data<-data_raw[, c("CountryName", "Date", "CumulativePositive", "CumulativeDeceased")]
	colnames(sel_data)<-c("country", "date", "cases", "deaths")
	sel_data$date<-as.Date(sel_data$date)
	sel_data[is.na(sel_data$deaths), ]$deaths<-0
	return (sel_data)
}

getTestData <- function(link) {
	data_raw<-read.csv(link,  na.strings = "", fileEncoding = "UTF-8-BOM")
	data_raw$country<-data_raw$countriesAndTerritories
	sel_data<-data_raw[, c("CountryName", "Date", "CumulativePositive", "CumulativeDeceased")]
	colnames(sel_data)<-c("country", "date", "cases", "deaths")
	sel_data$date<-as.Date(sel_data$date, format="%d/%m/%Y")
	return (sel_data)
}


getPartDataFromJH <- function(link, us=FALSE) {
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



getSingleCountryData <- function(data_all=NULL, country=NULL, source=NULL ) {
	ord_data<-NULL
	if (!is.null(data_all)) {
		if (country %in% data_all$country) {
			single_data<-data_all[grep(country, data_all$country), ]
			merge_data<-single_data
			death<-as.data.frame(diff(single_data$deaths))
			row.names(death)<-tail(row.names(single_data), -1)
			merge_data<-merge(tail(single_data, -1), death, by="row.names")
			merge_data$Row.names<-NULL
			merge_data$deaths<-NULL
			names(merge_data)<-c("country", "date","cases", "deaths")
			merge_data <- merge_data[order(merge_data$date),]
			rownames(merge_data) <- 1:nrow(merge_data)

			if (!grepl("PC", source, fixed = TRUE)) {
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
		}
	}
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

plotVAR<-function(predCFR) {
	cvdevcfr_t<-predCFR$cvdevcfr_t
	delay_time<-predCFR$delay
	delay_time
	cvdevcfr_t[is.na(cvdevcfr_t)]<-0
	locmin<-local.min.max(cvdevcfr_t)
	minim_idx<-match(locmin$maxima, cvdevcfr_t)
	title(main = paste0("Estimated delay is: ", delay_time), sub=paste0("Maxima", paste(minim_idx, collapse=",")))	

}

calcCFR<-function(dateshiftdiff=NULL, start_time=45, time_window=90, go_back=0, for_time=7, force_del=0, cfr_time=30) {
#Default
		defcor<-NA
		fitT7<-NA 
		fitdT7<-NA
		props<-NA
		fc_t<-NA
		fc<-NA
		cvdevcfr_t<-NA
		stdevcfr<-NA
		forecast<-NA
		for_min<-NA
		for_max<-NA
		perday<-NA
		perdaymin<-NA
		perdaymax<-NA

	results<-NULL
	if (!is.null(dateshiftdiff )) {
		if (is.null(for_time)) {
			for_time<-0
		}
		if (as.numeric(go_back > 0)) {
			dateshiftdiff<-head(dateshiftdiff, -as.numeric(go_back))
		}
		last_day<-tail(dateshiftdiff$date, 1)

		time_mavg<-7
		# make moving average
		fitdT7 <- getFit(dateshiftdiff$deaths, time_mavg)
		fitT7 <- getFit(dateshiftdiff$cases, time_mavg)

		### predict delay
		props_t<-c()
		fc_t<-c()
		stdevcfr_t<-c()
		cvdevcfr_t<-c()

		for(i in 1:start_time) {
			props_t <- tail(fitdT7, -i)/head(fitT7, -i)
		#	fc_t[i] <- mean(tail(props_t, n=time_window), trim = 0.10)
		#median absolute deviation or stdev without outliers?
			# remove outliers and calc average and stdev
			no_out<-tail(props_t, n=cfr_time)
			#no_out<-remove_outliers(tail(props_t, n=cfr_time))
			fc_t[i] <- mean(no_out, na.rm =TRUE)
			stdevcfr_t[i] <- sd(no_out, na.rm =TRUE)
			win_pos<-tail(tail(fitdT7, -i), time_window)
			win_deat<-tail(head(fitT7, -i), time_window)
			#cvdevcfr_t[i]<-ccf(win_pos, win_deat, na.action=na.omit)
			#cvdevcfr_t[i] <- cor.test(win_pos, win_deat)$estimate
		    cor.testres<-cor.test(win_pos, win_deat)
		    cor.val<-defcor
		    if (!is.na(cor.testres$p.value)) {
			    if (cor.testres$p.value<=0.05 && cor.testres$estimate > 0.5) {
			    	cor.val<-cor.testres$estimate
			    }
			}
			cvdevcfr_t[i] <- cor.val
			#cvdevcfr_t[i] <- mad(tail(props_t, n=time_window))
			#pos.betas <- ols.rgr(win_pos)
			#cvdevcfr_t[i] <- sse(b = pos.betas, dataset = win_deat)[,1]
		}

# check outliers
# inverted win_pos and win_deat
		#check no values
		if (all(is.na(cvdevcfr_t))) {
			delay_time<-NA
		} else {	
			delay_time<-which.max(cvdevcfr_t)
			#delay_time<-which.min(cvdevcfr_t)
		}
		if (force_del > 0) {
			delay_time = as.numeric(force_del)
		}

		forecast_time<-delay_time
		if (as.numeric(for_time) == 0) {
			forecast_time<-delay_time
		} else { 
			forecast_time<-as.numeric(for_time) 
		}

		if (!is.na(delay_time)) {
			props<-tail(fitdT7, -delay_time)/head(fitT7, -delay_time)*100
			fc<-fc_t[delay_time]
			stdevcfr<-stdevcfr_t[delay_time]
			forecast<-sum(dateshiftdiff$deaths)

			for_min <- forecast
			for_max <- for_min
			minstd<-fc-(1*stdevcfr)
			maxstd<-fc+(1*stdevcfr)
		
			if (is.na(minstd) ) {
				minstd<-0
			}
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

			perday<-format(round((forecast-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")
			perdaymin<-format(round((for_min-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")
			perdaymax<-format(round((for_max-sum(dateshiftdiff$deaths))/forecast_time, 0), big.mark=",")
		}

		results<-list("fitp7" = fitT7, 
			"fitd7" = fitdT7,
			"delay" = delay_time,
			"props" = props,
			"fc_t" = fc_t,
			"fc" = fc,
			"dateshiftdiff" = dateshiftdiff,
			"cvdevcfr_t" = cvdevcfr_t,
			"stdevcfr" = stdevcfr,
			"forecast" = forecast,
			"for_min" =for_min,
			"for_max" = for_max,		
			"perday" = perday,
			"perdaymin" = perdaymin,
			"perdaymax" = perdaymax,
			"for_time" = for_time
		)
	}
	return(results)
}

plotHistory<-function(country=NULL, single_country_data=NULL, start_time=45, time_window=90, forecast=7, force_del=0, time_CFR=30, gobackb=251, gobacka=0) {
	num<-0
	for(i in seq(gobackb+gobacka, gobacka, -forecast)) {
		predCFR<-calcCFR(single_country_data, start_time, time_window, i, forecast, force_del, time_CFR)
		res<-makeTable(predCFR)
		if (num==0) {
			table <- matrix(res$res)
		} else {
			table<-cbind(table, res$res)
		}
		num<-num+1
	}
	
	df<-as.data.frame(t(table))
	names(df)<-res$header
	date<-as.Date(as.vector(df$"Day"), format('%d-%m-%y'))
	date<-c(date, tail(date, n=1)+forecast)

	minfor<-c("NA", as.numeric(gsub(",", "", as.vector(df$"Min forecast"))))
	maxfor<-c("NA", as.numeric(gsub(",", "", as.vector(df$"Max forecast"))))
	lag<-c(as.vector(df$"Est lag"), "NA")
	CFR<-c(as.vector(df$"CFR"), "NA")
	CFR_stdev<-c(as.vector(df$"stdev"), "NA")
	deaths<-c(as.numeric(gsub(",", "", as.vector(df$"Deaths"))), "NA")
 	datafr<-data.frame("date" = date, "minfor" = as.numeric(minfor), "maxfor" = as.numeric(maxfor), "deaths" = as.numeric(deaths), "lag"= as.numeric(lag), "cfr"=as.numeric(CFR), "stdev"=as.numeric(CFR_stdev))

	#datafr<-data.frame("date" = date, "minfor" = as.numeric(minfor), "maxfor" = as.numeric(maxfor), "deaths" = as.numeric(deaths), "lag"= as.numeric(lag))
#print(datafr)

	library("ggplot2")
#	png_file<-paste0(country, "_", head(date, n=1), "_for_", forecast,  "_", start_time, "_", time_window, "_", forcedel, ".png")
#	png(png_file, height=500, width=1000)

	pl1 <- ggplot(datafr, aes(x=date)) + 
	  geom_ribbon(
		aes(ymin = minfor, ymax = maxfor), fill = "grey70") + 
	  geom_line(aes(y = deaths))+
	  geom_line(aes(y = minfor), color="red", linetype = "dashed")+
	  geom_line(aes(y = maxfor), color="red", linetype = "dashed")+
	  theme_classic() +
	  scale_x_date(breaks='1 month', labels = date_format("%b")) +
	  ggtitle(gsub("_", " ", country)) + xlab(NULL) +
	  theme(plot.title = element_text(hjust = 0.5))

	pl2<-ggplot(datafr, aes(date)) + xlab(NULL) +
	  geom_line(aes(y = lag)) + theme_classic() + theme(axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	  geom_hline(yintercept=7, linetype=2) 

	pl3<-ggplot(datafr, aes(date)) + xlab(NULL) +
		geom_ribbon(
		aes(ymin = cfr-stdev, ymax = cfr+stdev), fill = "grey70") + 
  geom_line(aes(y = cfr))+
  theme_classic() + theme(axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	   ylim(0,6) + 
           geom_hline(yintercept=2, linetype=2) 
	  

	plot_grid(pl1,pl2,pl3, ncol = 1, labels = c('A', 'B', 'C'), label_size = 12, rel_heights = c(2,1,1), align="hv")
}





plotTrend<-function(predCFR=NULL, force_ylim=0) {
	if (!is.null(predCFR)) {
		dateshiftdiff <-predCFR$dateshiftdiff
		props <-predCFR$props
		fitdT7 <-predCFR$fitd7
		fitT7 <-predCFR$fitp7
		delay_time<-predCFR$delay
		fc_t<-predCFR$fc_t
		country<-as.vector(head(predCFR$dateshiftdiff, 1)[,1])
	
		# max Y
		ylim_cases <- max(dateshiftdiff$cases)

		if (force_ylim > 0) {
			ylim_cases <- as.numeric(force_ylim)
		}
		ylim_deaths <- ylim_cases/10


		#subt1<-paste(format(last_day, "%d-%m-%y"), source_data, country, format(sum(dateshiftdiff$deaths), big.mark=","),"deaths.", , "The delay is", delay_time, "days.", "The CFR is:", round(fc_t[delay_time]*100, 2), "+/-", 1*round(stdevcfr*100, 2), "%", sep=" ")
		#subt2<-paste("Forecast of total deaths in ",forecast_time, "days", format(forecast, big.mark=","), "(", format(for_min, big.mark=","), "/", format(for_max, big.mark=","),").", "Forecast of daily deaths:", perday, "(", perdaymin, "/", perdaymax, ")", sep=" ")
		par(mar=c(10, 8, 4, 10) + 0.1)
		plot(zoo((dateshiftdiff$cases), dateshiftdiff$date), xaxt='n', yaxt='n', ylim=c(0,ylim_cases), type = c("p"), cex=0.5, lty=0, pch=16, ylab="", xlab="", col ="blue") 
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
	} else {
		NULL
	}
}

#plotTrend(predCFR)

plotCFR<-function(predCFR=NULL) {
	if (!is.null(predCFR)) {
		dateshiftdiff <-predCFR$dateshiftdiff
		cfr_perc <-predCFR$props
		delay_time<-predCFR$delay
		last_day<-tail(dateshiftdiff$date, 1)
	
		plot(zoo(cfr_perc, tail(dateshiftdiff$date, -delay_time)), xaxt='n',yaxs="i" , type = c("l"), ylim=c(0,10), xlab="Months", ylab="CFR (in %).", col ="red") 
		timeAxis(1, midmonth=TRUE, format="%b")
		abline(h=1, col="gray")
		abline(h=2, col="gray")
	} else {
		NULL
	}
}

makeRes<-function(predCFR=NULL, source, forday=7) {
	if (!is.null(predCFR)) {
		delay_time<-predCFR$delay
		dateshiftdiff <-predCFR$dateshiftdiff
		last_day<-format(tail(dateshiftdiff$date, 1), "%d-%m-%y")
		country<-as.vector(head(predCFR$dateshiftdiff, 1)[,1])
		tot_deaths<-format(sum(dateshiftdiff$deaths), big.mark=",")
		tot_pos<-format(sum(dateshiftdiff$cases), big.mark=",")
		CFR<-predCFR$fc
		forecast<-format(predCFR$forecast, big.mark=",")
		for_min<-format(predCFR$for_min, big.mark=",")
		for_max<-format(predCFR$for_max, big.mark=",")
		perday<-predCFR$perday 
		perdaymin<-predCFR$perdaymin 
		perdaymax<-predCFR$perdaymax 
		stdevcfr<-predCFR$stdevcfr

		paste0(country, ": ", tot_deaths, " deaths", ". Date: ", last_day,". The estimated lag between positives and deaths is: ", delay_time, ".<br>\n",
		"Dataset: ", source, ". The estimated CFR is: ", round(CFR*100, 2), " +/- ", 1*round(stdevcfr*100, 2), ".</br>\n",
		"Fatalities forecasted for the next ", forday, " days: ", forecast, " ( ", for_min, " / ", for_max, " ).\nDeaths forecasted per day: ", perday, " ( ", perdaymin, " / ", perdaymax, " ).\n")
	} else {
		NULL
	}
}

makeEU<-function(data_all) {
	eu_contries<-c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
	single_data.raw<-data_all[grep(paste(eu_contries, collapse="|"), data_all$country), ]
	single_data.raw$country<-NULL
	single_data.agg<-single_data.raw %>%
		group_by(date) %>% 
		summarize_all(sum, na.rm = TRUE)
	single_data.agg.df<-as.data.frame(single_data.agg)
	single_data.agg.df$country<-"EU"
	single_country_data<-single_data.agg.df[,c(4,1,2,3)]
	return(single_country_data)
}


makeTable<-function(predCFR) {
	delay_time<-predCFR$delay
	dateshiftdiff <-predCFR$dateshiftdiff
	last_day<-format(tail(dateshiftdiff$date, 1), "%d-%m-%y")
	country<-as.vector(head(predCFR$dateshiftdiff, 1)[,1])
	tot_deaths<-format(sum(dateshiftdiff$deaths), big.mark=",")
	tot_pos<-format(sum(dateshiftdiff$cases), big.mark=",")
	CFR<-predCFR$fc
	forecast<-format(predCFR$forecast, big.mark=",")
	for_min<-format(predCFR$for_min, big.mark=",")
	for_max<-format(predCFR$for_max, big.mark=",")
	perday<-predCFR$perday 
	perdaymin<-predCFR$perdaymin 
	perdaymax<-predCFR$perdaymax 
	stdevcfr<-predCFR$stdevcfr
	header = c("Country", "Day", "Deaths", "CFR", "stdev", "Est lag", "Forecast", "Min forecast", "Max forecast")
	res = c(country, last_day, tot_deaths, round(CFR*100, 2), 1*round(stdevcfr*100, 2), delay_time, forecast, for_min, for_max)
	return(list("header"=header, "res"=res))
}

getDataFromJH<-function(death_web, cases_web, single=FALSE){
	deat_jh<-getPartDataFromJH(death_web, single)
	pos_jh<-getPartDataFromJH(cases_web, single)
	colnames(deat_jh)<-c("country", "date", "deaths")
	colnames(pos_jh)<-c("country", "date", "cases")
	jh_data<-pos_jh
	jh_data$deaths<-deat_jh$deaths
	jh_data$country<-gsub("[()]","",as.character(jh_data$country))
	return(jh_data)

}

