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
forecast<-1
go_back<-1
predCFR<-calcCFR(single_country_data, 45, 90, go_back, forecast, 0, 30)
# predCFR, source, forecast
makeRes(predCFR, source, forecast)

#check total cases



