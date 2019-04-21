# README

Materials for Pteropod time series analysis, [link](http://sccwrp.shinyapps.io/pteropod_ts/dat_explr.Rmd)

Station comparisons, [link](http://sccwrp.shinyapps.io/pteropod_ts/station_comp.Rmd)

Figures, [link](http://fawda123.github.io/pteropod_ts/figures.html)

## Data summary

The relevant raw datasets are as follows: 

* `WOAC_allData_DS_forNina_nb.xlsx` pteropod biology and chemistry data

Processed data:

* `biodat.RData` processed biology data, only dissolution types as percentage of individuals were kept, abundances as no of inds/m3, average and standard deviation of sizes also retained

* `chmdat.RData` processed chemistry data, pCO2 is uatm, all other variables are umol/kg

* `simdat.RData` processed simulated data for 2008, six-hour time step for each station, selected only aragonite
