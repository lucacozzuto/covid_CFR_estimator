#Load the functions
source("functions.R")

# Get the data from the datasets
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
#predCFR<-calcCFR(single_country_data, MAXIMUM TIME TO SEARCH=45, WINDOW=90, GO BACK IN TIME=0, FORECAST=7,
#MANUAL SET OF THE LAG=0, CFR ESTIMATION TIME=30)


forecast<-1
go_back<-1

predCFR<-calcCFR(single_country_data, 45, 90, go_back, forecast, 0, 30)
# predCFR, source, forecast
makeRes(predCFR, source, forecast)

#check total cases



