#R --slave --args "US" 45 90 60000 "JH" 0 0 0 30 < plot_trend2.0.R 

#Using the functions
source("functions.R")
library(cowplot)
library(scales)

args<-commandArgs(TRUE)

country <- "EU"
start_time <- 45
time_window <- 90
force_ylim<-0
source <- "JRC"
forecast <- 7
go_back <- 0
force_del <- 0
for_time<-0
cfr_time <- 30

country <- args[1]
start_time <- as.numeric(args[2])
time_window <- as.numeric(args[3])
force_ylim <- as.numeric(args[4])
source_data <- args[5]
force_del <-as.numeric(args[6])
go_back <- as.numeric(args[7])
for_time <- as.numeric(args[8])
cfr_time <-as.numeric(args[9])


if (source_data == "JRC") {
	my_data<-getDataFromJRC(jrc_web)
} else if (source_data == "JH") {
	my_data<-getDataFromJH(death_web, cases_web)
} else if (source_data == "JHUSA") {
	my_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
} else if (source_data == "PC") {
	my_data<-getDataFromITA(ita_web)	
}

if (country == "EU") {
	single_country_data<-makeEU(my_data)	
} else {
	single_country_data<-getSingleCountryData(my_data, country, source)
}

#head(my_data)
predCFR<-calcCFR(single_country_data, start_time, time_window, go_back, for_time, force_del, cfr_time)
bname<-paste("CFR_", country, "_", source, sep="")

makeRes(predCFR, source, for_time)

png(paste0(bname, "_plot.png"), width=1200,  height=600)
plotTrend(predCFR)
dev.off()
png(paste0(bname, "_var.png"))
plotVAR(predCFR)
dev.off()
png(paste0(bname, "_cfr.png"))
plotCFR(predCFR)
dev.off()