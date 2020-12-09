#Using the functions
source("functions.R")
#R --slave --args "Italy" 45 90 "ECDC" "7" "91" "30" "outfile.txt"  < make_table.R 

args<-commandArgs(TRUE)

country <- args[1]
start_time <- as.numeric(args[2])
time_window <- as.numeric(args[3])
source <- args[4]
forecast <- as.numeric(args[5])
goback <- as.numeric(args[6])
time_CFR <- as.numeric(args[7])
outfile <- args[8]

if (source == "ECDC") {
	my_data<-getDataFromECDC(ecdc_web)
} else if (source == "JH") {
	my_data<-getDataFromECDC(death_web, cases_web)
} else if (source == "JHUSA") {
	my_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
} else if (source == "PC") {
	my_data<-getDataFromITA(ita_web)	
}

single_country_data<-getSingleCountryData(my_data, country, source)

num<-0
for(i in seq(91, 0, -forecast)) {
	predCFR<-calcCFR(single_country_data, start_time, time_window, i, forecast, 0, time_CFR)
#	if (forecast>predCFR$delay) {
#		predCFR$forecast<-NA
#		predCFR$for_min<-NA
#		predCFR$for_max<-NA
#	} 
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
write.table(file=outfile, df, row.names=FALSE)




