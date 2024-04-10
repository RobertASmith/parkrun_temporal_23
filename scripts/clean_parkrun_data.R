# R script to clean data
rm(list = ls())

library(data.table)
library(zoo)
library(tidyverse)

#===============================#
# INDEX OF MULTIPLE DEPRIVATION #
#===============================#

lsoa_imd <- data.table::fread("data/raw/IMD_data_2019.csv")[
  substr(`LSOA code (2011)`, start = 1, stop = 1) == "E" # restrict to England.
  ,.(lsoa = `LSOA code (2011)`,
     imd_score = `Index of Multiple Deprivation (IMD) Score`,
     imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
     total_pop = `Total population: mid 2015 (excluding prisoners)`,
     perc_non_working_age = 1 - (`Working age population 18-59/64: for use with Employment Deprivation Domain (excluding prisoners)`/`Total population: mid 2015 (excluding prisoners)`))
] # only retain the columns we need

# weird - there exist 10 locations in which the population working age exceeds total, in this case make 0% non working age
lsoa_imd$perc_non_working_age[lsoa_imd$perc_non_working_age<0] <- 0

# proportion to percent
lsoa_imd$perc_non_working_age = lsoa_imd$perc_non_working_age*100



#collapse IMD deciles into quintiles
lsoa_imd <- lsoa_imd |>
  dplyr::mutate(imd_q5 = case_when(imd_decile == "1"| imd_decile == "2" ~ "Most deprived 20%",
                            imd_decile == "3"| imd_decile == "4" ~ "2",
                            imd_decile == "5"| imd_decile == "6" ~ "3",
                            imd_decile == "7"| imd_decile == "8" ~ "4",
                            imd_decile == "9"| imd_decile == "10" ~ "Least deprived 20%",))

imd_levels <- c("Least deprived 20%","4", "3", "2", "Most deprived 20%")

levels(lsoa_imd$imd_q5) <-  imd_levels

lsoa_imd$imd_q5 <- factor(x = lsoa_imd$imd_q5,
                          levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))



#===============================#
#          FINISHER DATA
#===============================#

# Read in all csv files in event-data
participation_data_folder <- "data/raw/eventdata"

# get a vector of all of the data files in the folder
v_parkrun_runs_csvs <- list.files(path = participation_data_folder,
                                  full.names = T)

# read in the files and store in a list.
l_part_data <- lapply(X = v_parkrun_runs_csvs,
                      FUN = data.table::fread)

# unlist the data
dt_runs <- do.call(rbind, l_part_data)

# change the column name for count
names(dt_runs)[names(dt_runs) == "count(athleteid)"] <- "finishers"
names(dt_runs)[names(dt_runs) == "lsoa11"] <- "lsoa"

# select LSOAs in England only
dt_runs <- dt_runs[substr(dt_runs$lsoa, start = 1, stop = 1) == "E"]

# convert the eventdate variable to a date.
dt_runs$date <- zoo::as.Date(x = dt_runs$eventdate, format="%Y-%m-%d")

# restrict to saturday events only
dt_runs$day <- base::weekdays(x = dt_runs$date)
dt_runs     <- dt_runs[dt_runs$day == "Saturday"]

#save in workspace as runs_full for later use
runs_full <- dt_runs

#add imd data to finisher df
dt_runs <- dplyr::left_join(dt_runs, lsoa_imd, by = "lsoa")

#summarise n finishers for each week stratified by imd quintile
dt_runs <- dt_runs |>
  dplyr::group_by(date, imd_q5) |>
  dplyr::summarise(finishers = sum(finishers)) |>
  dplyr::filter(is.na(imd_q5) == F)


#fill in missing weeks#
dt_runs <- dt_runs[order(x = dt_runs$date),]

all_weeks <- data.table::data.table(date = rep(seq(from = min(dt_runs$date),
                                       to = max(dt_runs$date), 7),
                                       each = 5))

all_weeks$imd_q5 <- rep(imd_levels,
                        times = nrow(all_weeks)/5)

dt_runs <- dplyr::left_join(x = all_weeks,
                     y = dt_runs,
                     by = c("date", "imd_q5"))

#add total popolation for each imd quintile for rate calculations etc.
df_pop <- lsoa_imd |>
  dplyr::group_by(imd_q5) |>
  dplyr::summarise(imd_pop = sum(total_pop))

dt_runs <- dt_runs |>
  dplyr::mutate(total_pop = dplyr::case_when(imd_q5 == "Most deprived 20%" ~ df_pop$imd_pop[1],
                             imd_q5 == "2" ~ df_pop$imd_pop[2],
                             imd_q5 == "3" ~ df_pop$imd_pop[3],
                             imd_q5 == "4" ~ df_pop$imd_pop[4],
                             imd_q5 == "Least deprived 20%" ~ df_pop$imd_pop[5]))

##Discard pre-2015 data: trend changed over time
dt_runs <- dt_runs[dt_runs$date >"2015-01-01"]





 #===============================#
 #          WEEKLY N EVENTS
 #===============================#

 #calculate weekly number events run
 df_events <- runs_full |>
   dplyr::group_by(eventdate) |>
   dplyr::distinct(eventname, .keep_all = T) |>
   dplyr::summarise(n = n()) |>
   dplyr::select("date" = eventdate, "n_events" = n) |>
   dplyr::filter(date > "2015-01-01") |> #keep only post 2015 data
   dplyr::mutate(date = as_date(date))

 agg_data <- dplyr::left_join(x = dt_runs,
                              y = df_events,
                              by = "date")

#=========#
# SAVE DATASETS
#=========#

 #save for analysis
 saveRDS(object = agg_data,
         file = "data/clean/dt_finisher_ts.rds")
