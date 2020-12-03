<img align="right" href="https://biocore.crg.eu/" src="https://github.com/CRG-CNAG/BioCoreMiscOpen/blob/master/logo/biocore-logo_small.png" />

# Covid CFR estimator
[![DOI](https://zenodo.org/record/4304955.svg)](https://zenodo.org/record/4304955)


Estimating the case fatality rate (CFR) of an ongoing pandemics is a complex problem.
On average a person die of Covid19 after two weeks from the contagion, but delay in reporting the number of positives or deaths may alter this correlation.

This app infers the lag between the trend of people positives to Covid19 and the deaths in a particular moment.
A shorter lag can indicate a inefficient testing, while a longer one can indicate delay in reporting the deaths.
This lag is then used to estimate the CFR for forecasting the number of future fatalities from the number of positives.


You can select 3 different datasets offered by: 
* The John Hopkins resource centre
* The European Centre for Disease Prevention and Control (ECDC)
* The Italian Protezione Civile for the Italian regions.
After selecting the dataset you can choose a country / region.

The shiny app is currently at

https://lucacozzuto.shinyapps.io/shiny/

