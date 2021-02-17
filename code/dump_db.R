#Using the functions
source("functions.R")
library(cowplot)
library(scales)

#R --slave --args 45 60 "JRC" "7" "30" < dump_db.R 

args<-commandArgs(TRUE)

start_time <- as.numeric(args[1])
time_window <- as.numeric(args[2])
source <- args[3]
forecast <- as.numeric(args[4])
time_CFR <- as.numeric(args[5])
goback <- as.numeric(args[6])

if (source == "JRC") {
	my_data<-getDataFromJRC(jrc_web)
} else if (source == "JH") {
	my_data<-getDataFromJH(death_web, cases_web)
} else if (source == "JHUSA") {
	my_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
} else if (source == "PC") {
	my_data<-getDataFromITA(ita_web)	
}

countries<-as.vector(unique(my_data$country))
print(countries)

results <- data.frame(Date=as.Date(character()),
                 Country=character(), 
                 LagTime=integer(),
                 CFR=numeric(),
                 deaths=integer(),
                 deathForecast=integer(),
                 deathMin=integer(),
                 deathMax=integer()
                 ) 
                 
resnames<-names(results)

for(country in countries) {
	single_country_data<-getSingleCountryData(my_data, country, source)
	#print(country)
	predCFR<-calcCFR(single_country_data, start_time, time_window, goback, forecast, 0, time_CFR)
	single_res<-(data.frame(tail(predCFR$dateshiftdiff, 1)["date"], country, predCFR$delay, round(predCFR$fc*100,2), sum(predCFR$dateshiftdiff["deaths"]), predCFR$forecast, predCFR$for_min, predCFR$for_max))
	results <-rbind(results, single_res)
}
names(results)<-resnames

outname<-paste(source, start_time, time_window, forecast, time_CFR, Sys.Date()-goback, sep="_")
outfile<-paste0(outname, ".txt")

write.table(results, file=outfile, row.names=FALSE, sep="\t")
