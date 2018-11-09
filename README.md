# README

Materials for Pteropod time series analysis, [link](http://162.243.131.102:3838/pteropod_ts/dat_explr.Rmd)

Station comparisons, [link](https://fawda123.github.io/pteropod_ts/station_comp)

## Data summary

The relevant raw datasets are as follows: 

* `WOAC_allData_DS_forNina_nb.xlsx` pteropod biology and chemistry data

Processed data:

* `biodat.RData` processed biology data, only dissolution types as percentage of individuals were kept, abundances as no of inds/m3, average and standard deviation of sizes also retained

* `chmdat.RData` processed chemistry data, pCO2 is uatm, all other variables are umol/kg

* `simdat.RData` processed simulated data for 2008, six-hour time step for each station, selected only aragonite

## Hypotheses

The analysis is structured to evaluate and provide evidence in support of the following hypotheses:

1) Exposure of pteropod cohorts to OA stress varies over time and space in the Puget Sound
     * Maps of OA parameters in the Puget sound will show variation on OA stress
     * These maps can be qualitatively compared to pteropod response measures of OA stress

2) Pteropod physiological response to OA conditions varies as a function of stressor exposure
     * Pteropod response will be most severe at stations with highest stress (i.e., aragonite under-saturation)
     * Total exposure to stress will be integrated within each cohort, such that adults assessed at the end of the growing season are expected to exhibit the strongest response to OA stressors at locations where OA stress is highest
     
3) Stressor frequency and magnitude can be differentially characterized and meaningfully compared to variation in OA response of pteropods
    * Pteropod response to stressors will vary depending on the type of exposure, characterized by acute, chronic, variable, or none
    * Temporal history of exposure from moored data will determine pteropod response

