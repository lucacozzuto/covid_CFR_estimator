#Using the functions
source("functions.R")
library(cowplot)
library(scales)

#R --slave --args "Italy" 45 90 "JRC" "7" "1" "151" "30" "0" < make_table.R 

args<-commandArgs(TRUE)

country <- "Italy"
start_time <- 45
time_window <- 90
source <- "JH"
forecast <- 7
gobacka <- 1
gobackb <- 73
time_CFR <- 30
forcedel<-0
#outfile <- "out.txt"

country <- args[1]
start_time <- as.numeric(args[2])
time_window <- as.numeric(args[3])
source <- args[4]
forecast <- as.numeric(args[5])
gobacka <- as.numeric(args[6])
gobackb <- as.numeric(args[7])
time_CFR <- as.numeric(args[8])
forcedel <- as.numeric(args[9])


if (source == "JRC") {
	my_data<-getDataFromJRC(jrc_web)
} else if (source == "JH") {
	my_data<-getDataFromJH(death_web, cases_web)
} else if (source == "JHUSA") {
	my_data<-getDataFromJH(death_web_US, cases_web_US, TRUE)
} else if (source == "PC") {
	my_data<-getDataFromITA(ita_web)	
}

single_country_data<-getSingleCountryData(my_data, country, source)
a<-plotHistory(country, single_country_data, start_time, time_window, forecast, forcedel, time_CFR)
png("ocazz.png", height=500, width=1000)
print(a)
dev.off()

num<-0
for(i in seq(gobackb, gobacka, -forecast)) {
	predCFR<-calcCFR(single_country_data, start_time, time_window, i, forecast, forcedel, time_CFR)
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
png_file<-paste0(country, "_", head(date, n=1), "_for_", forecast,  "_", start_time, "_", time_window, "_", forcedel, ".png")
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
            axis.ticks.x = element_blank()) +
  geom_hline(yintercept=forecast, linetype=2) 

plot_grid(pl1,pl2, ncol = 1, labels = c('A', 'B'), label_size = 12, rel_heights = c(2,1), align="hv")

dev.off()

outfile <- paste0(country, "_", head(date, n=1), "_for_", forecast, "_", start_time, "_", time_window, ".txt")
write.table(file=outfile, df, row.names=FALSE)

png_file2<-paste0(country, "_", head(date, n=1), "_lag_", forecast,  "_", start_time, "_", time_window, ".png")
png(png_file2, height=500, width=1000)

my_day1<-as.Date("2020-10-31")
cut<-7
#my_day1<-as.Date("2020-09-25")
#my_day2<-as.Date("2020-10-04")

ggplot(datafr, aes(date)) + xlab(NULL) +
  geom_line(aes(y = lag)) + theme_classic() + 
  scale_x_date(breaks='1 month', labels = date_format("%b")) +
  #geom_vline(xintercept=as.numeric(my_day1), linetype=4) +
  geom_hline(yintercept=cut, linetype=2) +
  #geom_vline(xintercept=as.numeric(my_day2), linetype=4) +
  ggtitle(gsub("_", " ", country)) + xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))
   
dev.off()
