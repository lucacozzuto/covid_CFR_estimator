#Using the functions
source("functions.R")
library(cowplot)
library(scales)

#R --slave --args "United_States_of_America" 45 90 "ECDC" "7" "1" "151" "30"  < make_table.R 

args<-commandArgs(TRUE)

country <- "Spain"
start_time <- 45
time_window <- 90
source <- "ECDC"
forecast <- 7
gobacka <- 1
gobackb <- 151
time_CFR <- 30
#outfile <- "out.txt"

country <- args[1]
start_time <- as.numeric(args[2])
time_window <- as.numeric(args[3])
source <- args[4]
forecast <- as.numeric(args[5])
gobacka <- as.numeric(args[6])
gobackb <- as.numeric(args[7])
time_CFR <- as.numeric(args[8])


if (source == "ECDC") {
	my_data<-getDataFromECDC(ecdc_web)
} else if (source == "JH") {
	my_data<-getDataFromJH(death_web, cases_web)
} else if (source == "JHUSA") {
	my_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
} else if (source == "PC") {
	my_data<-getDataFromITA(ita_web)	
}

single_country_data<-getSingleCountryData(my_data, country, source)

num<-0
for(i in seq(gobackb, gobacka, -forecast)) {
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
date<-as.Date(as.vector(df$"Day"), format('%d-%m-%y'))
date<-c(date, tail(date, n=1)+forecast)

minfor<-c("NA", as.numeric(gsub(",", "", as.vector(df$"Min forecast"))))
maxfor<-c("NA", as.numeric(gsub(",", "", as.vector(df$"Max forecast"))))
lag<-c(as.vector(df$"Est lag"), "NA")
deaths<-c(as.numeric(gsub(",", "", as.vector(df$"Deaths"))), "NA")

datafr<-data.frame("date" = date, "minfor" = as.numeric(minfor), "maxfor" = as.numeric(maxfor), "deaths" = as.numeric(deaths), "lag"= as.numeric(lag))

library("ggplot2")
png_file<-paste0(country, "_", head(date, n=1), "_for_", forecast, ".png")
png(png_file, height=500, width=1000)

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
            axis.ticks.x = element_blank()) 

plot_grid(pl1,pl2, ncol = 1, labels = c('A', 'B'), label_size = 12, rel_heights = c(2,1), align="hv")

dev.off()

outfile <- paste0(country, "_", head(date, n=1), "_for_", forecast, ".txt")
write.table(file=outfile, df, row.names=FALSE)


