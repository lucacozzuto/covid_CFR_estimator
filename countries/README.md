#plot_trend

For executing the script you can use this command line

R --slave --args "US" 120 90 "" "ECDC" "" "" < plot_trend2.0.R 

#Libraries needed 

library(dplyr)

library(stringr)

library("EnvStats")

library("spatialEco")

library("berryFunctions")

library("zoo")

Parameters:
1. Country (string).
2. Time to be added to the window time for checking the delay (integer). You start searching 120 + 90 in the past in the example.
3. Scanning window (integer).
4. Maximum value of positives to be shown for Y axis (empty or number). If empty is automatically adjusted.
5. Data source. Currenlty we support ECDC, Jhon Hopkins (JH) and Italian Protezione Civile (PC) for Italian regions.
6. Force delay. (empty or number) You force using a fixed value for delay. Useful when multple minima are reported. 
7. Go back in time (empty or number). You move the last day back in time.

#Adding a docker image.
