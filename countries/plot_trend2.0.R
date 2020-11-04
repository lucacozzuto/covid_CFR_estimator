#R --slave --args "US" 120 90 20000 < plot_trend2.0.R 

args<-commandArgs(TRUE)

#country <- "Italy"
#start_time <- 30
#time_window <- 21
#pad <- 1000


country <- args[1]
start_time <- as.numeric(args[2])
time_window <- as.numeric(args[3])
pad <- as.numeric(args[4])

# get the data from John Hopkins
death_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
cases_web <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

if (country == "UK") {
	country <- "United Kingdom"
}

getDataFromWeb <- function(link) {
	data_raw<-read.csv(link)
	# aggregate data and remove not useful ones
	data_raw$Province.State<-NULL
	data_raw$Lat<-NULL
	data_raw$Long<-NULL
	library(dplyr)
	data_raw.agg<-data_raw %>%
		group_by(Country.Region) %>% 
		summarize_all(sum, na.rm = TRUE)
	data_raw.agg.df<-as.data.frame(data_raw.agg)
	row.names(data_raw.agg.df)<-data_raw.agg.df$Country.Region
	data_raw.agg.df$Country.Region<-NULL
	return(data_raw.agg.df)
}

death_data<-getDataFromWeb(death_web)
cases_data<-getDataFromWeb(cases_web)

getSingleCountryData <- function(data_all, country ) {
	single_data<-colSums(data_all[grep(country, row.names(data_all)), ])
	return (single_data)
}

deaths_one_country<-getSingleCountryData(death_data, country)
cases_one_country<-getSingleCountryData(cases_data, country)

creatingPerDayDiff <- function (data_one_country) {
	shift2tot<-t(data.frame(data_one_country))
	shift3tot<-shift2tot
	tdays<-seq(1,length(shift2tot))
	maxtday<-max(tdays)
	rowtnum<-length(shift2tot[, maxtday])
	shift3tot<-shift2tot[, -maxtday]
	shift3tot<-t(as.data.frame(shift3tot))
	colnames(shift3tot)<-seq(2,maxtday)
	shift3tot<-cbind("1" = 0, shift3tot)
	shiftdifft<-shift2tot-shift3tot
	shiftdifft[shiftdifft<0]<-0
	return(shiftdifft)
}

deaths_diff<-creatingPerDayDiff(deaths_one_country)
cases_diff<-creatingPerDayDiff(cases_one_country)


ylim_cases <- max(cases_diff)+pad
ylim_deaths <- ylim_cases/10


time_mavg<-7

getFit <- function(data_to_fit, time_mavg) {
	require(pracma)
	fitted_data <- movavg(as.numeric(data_to_fit["data_one_country", ]), time_mavg, type=c("s"))
	return(fitted_data)
}

fitdT7 <- getFit(deaths_diff, time_mavg)
fitT7 <- getFit(cases_diff, time_mavg)

library(stringr)
dateshiftdiff<-as.data.frame(t(cases_diff))
names(dateshiftdiff)<-c("tot")
dateshiftdiff$deaths<-as.vector(t(deaths_diff))
dateshiftdiff$date<-as.Date(str_replace_all(str_replace(row.names(dateshiftdiff), "X", ""), '\\.', "-"), format="%m-%d-%y")

### check better delay
#start_time<-21
#time_window<-14
props_t<-c()
fc_t<-c()
stdevcfr_t<-c()
cvdevcfr_t<-c()

library("EnvStats")
#std <- function(x) sd(x)/sqrt(length(x))

for(i in 1:start_time) {
    props_t <- tail(fitdT7, -i)/head(fitT7, -i)
    fc_t[i] <- mean(tail(props_t, n=time_window), trim = 0.10)
    stdevcfr_t[i] <- mad(tail(props_t, n=time_window))
    win_pos<-tail(tail(fitdT7, -i), time_window)
    win_deat<-tail(head(fitT7, -i), time_window)
	cvdevcfr_t[i] <- cor.test(win_pos, win_deat)$p.value
}

#tail(tail(fitdT7, -delay_time), time_window)/tail(head(fitT7, -delay_time), time_window)


delay_time<-which.min(cvdevcfr_t)
forecast_time<-delay_time
min(cvdevcfr_t)


fname2<-paste("VAR_", country, "_", format(Sys.time(), "%d-%m-%y"),  ".png", sep="")
png(fname2)
plot(log(cvdevcfr_t))
dev.off()

paste0("Estimated delay is: ", delay_time)

props<-tail(fitdT7, -delay_time)/head(fitT7, -delay_time)*100
fc<-fc_t[delay_time]
stdevcfr<-stdevcfr_t[delay_time]

forecast<-sum(dateshiftdiff$deaths)
for_min <- forecast
for_max <- for_min

minstd<-fc-stdevcfr
maxstd<-fc+stdevcfr

if (minstd < 0) {
	minstd<-0
}

for(i in 1:forecast_time) {
	forecast = forecast + fitT7[length(props)+i]*fc
	#print(forecast)
	for_min = for_min + fitT7[length(props)+i]*(minstd)
	for_max = for_max + fitT7[length(props)+i]*(maxstd)
}
for_min = round(for_min)
for_max = round(for_max)

diff_for<-round(forecast)-sum(dateshiftdiff$deaths)
library("berryFunctions")
library("zoo")
fname<-paste("trend_", country, "_", format(Sys.time(), "%d-%m-%y"),  ".png", sep="")
png(fname, width=1200,  height=600)
par(mar=c(10, 8, 4, 10) + 0.1)

subt1<-paste(country, format(sum(dateshiftdiff$deaths), big.mark=","),"deaths. The delay is", delay_time, "days", "CFR is:", round(fc_t[delay_time]*100, 2), "+/-", round(stdevcfr*100, 2), "%", sep=" ")
subt2<-paste("Forecast in", forecast_time, "days", format(for_min, big.mark=","), "/", format(for_max, big.mark=","), sep=" ")
#subt2<-""
plot(zoo((dateshiftdiff$tot), dateshiftdiff$date), xaxt='n', yaxt='n', main = paste(c(subt1, subt2), sep=""), ylim=c(0,ylim_cases), type = c("p"), cex=0.5, lty=0, pch=16, ylab="", xlab="", col ="blue") 
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

dev.off()

cfr<-(tail(fitdT7, -delay_time)/head(fitT7, -delay_time))
fname<-paste("CFR_", country, "_", format(Sys.time(), "%d-%m-%y"),  ".png", sep="")

cfr_perc<-cfr*100

png(fname)
plot(zoo(cfr_perc, tail(dateshiftdiff$date, -delay_time)), xaxt='n',yaxs="i" , type = c("l"), ylim=c(0,10), xlab="Months", ylab="CFR (in %).", col ="red") 
timeAxis(1, midmonth=TRUE, format="%b")
abline(h=1, col="gray")
abline(h=2, col="gray")
dev.off()

#library("vioplot")
#png(fname2)
#vioplot(tail(cfr_perc, 60))
#sdev.off()

#avecfr<-round(mean(tail(cfr, 60))*100, 2)
#medcfr<-round(median(tail(cfr, 60))*100, 2)
#stdevcfr<-round(sd(tail(cfr, 60))*100, 2)
paste("Average CFR is: ", fc_t[delay_time]*100, "% +/- ", stdevcfr_t[delay_time]*100, "%")
#paste("Median CFR is: ", medcfr)
