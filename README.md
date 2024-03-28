# parkrun_temporal_23
Code for a paper which asseses the long-term impact of the pandemic on parkrun participation using interrupted time series analysis.


**Installation**
1. Install R studio
2. Clone this repository: https://github.com/RobertASmith/parkrun_temporal_23
3. Open parkrun_temporal_23.Rproj in R studio
4. There are two scripts:
   * Clean_parkrun_data.R: cleans raw data files, outputs datasets ready for analysis. This has been run already and aggregated data 
     has been uploaded to the repository
  * time_series.R: runs descriptive and statistical analyses. This script should be run. Requires two datasets which are included in the repository:
       - IMD_data_2019: CSV from Indices of Deprivation report 2019, UK government (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) available in line with an open government licence         
         (https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/);
       - dt_finisher_ts: aggregated data detailing number of finishers each week by IMD and number of events held each week. 
6. Figures and tables can be found in 'outputs'.
