#Using the functions
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

#Example for loading a single country
source<-"ECDC"
#get the predictions
single_country_data<-getSingleCountryData(ecdc_data, "Italy", source)
#predCFR<-calcCFR(single_country_data, start_time, time_window, go_back, forecast_time, force_delay, cfr_estimation_time)

num<-0
forecast<-7
for(i in seq(91, 0, -forecast)) {
	predCFR<-calcCFR(single_country_data, 45, 90, i, forecast, 0, 30)
	if (forecast>predCFR$delay) {
		predCFR<-NA
	} 
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


# predCFR, source, forecast
#res <- makeTable(predCFR)
#check total cases




