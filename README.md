# The Long-Term Effect of the Coronavirus Pandemic on parkrun Participation: An Interrupted Time Series Analysis. Under review. 2024

## Authors
Oscar Rousham, Dr. Robert A. Smith \*, Dr. Helen Quirk and Prof. Elizabeth Goyder

\*corresponding author: robert.smith@sheffield.ac.uk

## Abstract

> 
>Background  
>The growth of parkrun between 2004 and 2019 has been heralded as a success story for public health as a result of its physical activity and wellbeing benefits for participants. However, parkrun was not immune from the COVID-19 pandemic - with UK events cancelled from March 2020 to July 2021. This study explores the lasting impact of the pandemic on parkrun participation to February 2023, and its implications across the socioeconomic spectrum.
>
>Methods  
>The study combines aggregated parkrun weekly finisher data from 32,470 Lower Layer Super Output Areas (LSOA) in England from January 2015 to February 2023 with Office of National Statistics (ONS) data on population and deprivation. Interrupted time series analysis using segmented Poisson regression models was used to estimate the immediate change in parkrun participation and the change in the rate of growth following the pandemic. Models were fitted for each Index of Multiple Deprivation (IMD) quintile separately to assess whether this effect differed by socioeconomic deprivation.
>
>Results  
>Visualisation and interrupted time series analysis showed a significant and long-term decrease in parkrun participation following the pandemic period. This was consistent across all IMD quintiles, indicating enduring health inequalities. Between March 2020 and February 2023, a total of 12,928,751 fewer parkrun finishes are estimated to have occurred relative to what would have occurred in the absence of the pandemic.
> 
>Conclusion  
>The reduction in parkrun participation following the pandemic is likely to have negatively impacted wellbeing in would-be participants. Going forwards, policymakers must make the difficult trade-off between the long-term health and social implications of restricting outdoor physical activity events against the benefits associated with a reduction in infectious disease transmission.

## The code

**Setup**
1. Install R studio
2. Clone this repository: https://github.com/RobertASmith/parkrun_temporal_23
3. Open parkrun_temporal_23.Rproj in R studio

**Replication**
There are two R scripts, `clean_parkrun_data.R` and `time_series.R`. 
   * Clean_parkrun_data.R cleans raw data files, outputs datasets ready for analysis. This has been run already and aggregated data has been uploaded to the repository. This is just for reference as unaggregated data is not publicly available.
   * time_series.R: runs descriptive and statistical analyses. This script can be run to replicate the analysis. The script requires two datasets which are included in their correct locations in the repository:
       - [IMD_data_2019.csv](/data/raw): CSV from Indices of Deprivation report 2019, [(UK government,2019)](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) available in line with an [open government licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/);
       - [dt_finisher_ts.rds](/data/clean): aggregated data detailing number of finishers each week by IMD and number of events held each week. Shared with permission from parkrun UK.

All of the outputs from the analysis can be found in the (outputs)[/outputs/] folder.

See a list of packages with versions below.


### Acknowledgements, Funding & Competing interests

Competing interests:
H.Q. is Deputy Chair of the parkrun Research Board.

Funding: 
R.S. & O.R. are jointly funded by the Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science [108903] and the University of Sheffield.

The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript. The views, thoughts, and opinions expressed in this report belong solely to the authors, and do not reflect the position of the funder or parkrun.

Acknowledgements:   
We would like to thank Mike Graney (Head of Analysis at parkrun) and Chrissie Wellington (Global Head of Health and Wellbeing at parkrun) for providing area level parkrun participation data.

Data:
We provide our cleaned data by IMD quintile - for access to the raw data utilised in this code please contact Mike Graney at parkrunUK.

## Dependencies





