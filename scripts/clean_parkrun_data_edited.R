# R script to clean data
rm(list = ls())

# install.packages("data.table)
# install.packages("curl)

library(data.table)
library(raster)
library(geosphere)
library(sf)
library(zoo)
library(tidyverse)

`%nin%` <- Negate(`%in%`)

source("R/get_min_dist.R")
source("R/Exclusion_criteria.R")

#===============================#
# INDEX OF MULTIPLE DEPRIVATION #
#===============================#

lsoa_imd <- data.table::fread("https://raw.githubusercontent.com/RobertASmith/parkrun_temporal/master/rawdata/IMD_data.csv")[
  substr(`LSOA code (2011)`, start = 1, stop = 1) == "E" # restrict to England.
  ,.(lsoa = `LSOA code (2011)`,
     imd_score = `Index of Multiple Deprivation (IMD) Score`,
     imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
     total_pop = `Total population: mid 2012 (excluding prisoners)`,
     perc_non_working_age = 1 - (`Working age population 18-59/64: for use with Employment Deprivation Domain (excluding prisoners)`/`Total population: mid 2012 (excluding prisoners)`))
] # only retain the columns we need

# weird - there exist 10 locations in which the population working age exceeds total, in this case make 0% non working age
lsoa_imd$perc_non_working_age[lsoa_imd$perc_non_working_age<0] <- 0

# proportion to percent
lsoa_imd$perc_non_working_age = lsoa_imd$perc_non_working_age*100

#create imd quintiles from imd score
imd_quintiles_cuts <- quantile(x = lsoa_imd$imd_score,
                               probs = seq(0, 1, 0.2), na.rm= T)

lsoa_imd$imd_q5 <- cut(x = lsoa_imd$imd_score,
                              imd_quintiles_cuts,
                              include.lowest = T)

imd_levels <- c("Least deprived 20%","4", "3", "2", "Most deprived 20%")

levels(lsoa_imd$imd_q5) <-  imd_levels

lsoa_imd$imd_q5 <- factor(x = lsoa_imd$imd_q5,
                                 levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))




#===============================#
#           ETHNICITY           #
#===============================#

lsoa_ethnicity <- fread(input = "https://raw.githubusercontent.com/RobertASmith/parkrun_temporal/master/rawdata/Ethnicity_data/LSOA_Ethnicity.csv",
                       stringsAsFactors = F)
lsoa_ethnicity <- lsoa_ethnicity[,
                                .(lsoa = `geography code`,
                                  ethnic_density = 1- (`Sex: All persons; Age: All categories: Age; Ethnic Group: White: English/Welsh/Scottish/Northern Irish/British; measures: Value`/
                                                         `Sex: All persons; Age: All categories: Age; Ethnic Group: All categories: Ethnic group; measures: Value`)
                                )]
lsoa_ethnicity <- lsoa_ethnicity[!(grepl("W",lsoa_ethnicity$lsoa)),]
# proportion to percent
lsoa_ethnicity$ethnic_density <- lsoa_ethnicity$ethnic_density*100

#===============================#
#        EVENT LOCATIONS        #
#===============================#

event_locations <- data.table::fread("https://raw.githubusercontent.com/RobertASmith/parkrun_temporal/master/rawdata/event_info_20181231.csv") # read in data
event_locations$Estblsh <- as.Date(x = event_locations$Estblsh, "%d/%m/%Y") # convert date
event_locations$month_year <- substr(event_locations$Estblsh, start =  1, stop = 7) # get month and year


#===============================#
#     POPULATION DENSITY        #
#===============================#

lsoa_density <- fread("https://raw.githubusercontent.com/RobertASmith/parkrun_temporal/master/rawdata/Mid-2017%20Population%20Density.csv",
                     stringsAsFactors = F)[grep(pattern = "E",`Code`),
                            .(lsoa = `Code`,
                              pop_density = `People per Sq Km`)]
# convert ppl / km^2 to 1,000* ppl / km^2
lsoa_density$pop_density <- as.numeric(gsub(",", "", lsoa_density$pop_density))/1000

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

# convert the eventdate varaible to a date.
dt_runs$date <- as.Date(x = dt_runs$eventdate, format="%Y-%m-%d")

# restrict to saturday events only
dt_runs$day <- weekdays(x = dt_runs$date)
dt_runs     <- dt_runs[dt_runs$day == "Saturday"]

#apply exclusion criteria #1: discontinued parkruns
dt_runs <- discontinued(dt_runs)

#extract cancelled dates (i.e. adverse weather days) for later exclusion
cancelled_events <- cancelled(dt_runs)

#add imd data to finisher df
dt_runs <- left_join(dt_runs, lsoa_imd, by = "lsoa")

#summarise n finishers for each week stratified by imd quintile
dt_runs <- dt_runs |>
  group_by(date, imd_q5) |>
  summarise(finishers = sum(finishers)) |>
  filter(is.na(imd_q5) == F)


##fill in missing weeks##
dt_runs <- dt_runs[order(x = dt_runs$date),]

all_weeks <- data.table(date = rep(seq(min(dt_runs$date),
                                       max(dt_runs$date), 7),
                                       each = 5))

all_weeks$imd_q5 <- rep(imd_levels,
                        times = nrow(all_weeks)/5)

dt_runs <- left_join(x = all_weeks,
                     y = dt_runs,
                     by = c("date", "imd_q5"))

#add total popolation for each imd quintile for rate calculations etc.
df_pop <- lsoa_imd |>
  group_by(imd_q5) |>
  summarise(imd_pop = sum(total_pop))

dt_runs <- dt_runs |>
  mutate(total_pop = case_when(imd_q5 == "Most deprived 20%" ~ df_pop$imd_pop[1],
                             imd_q5 == "2" ~ df_pop$imd_pop[2],
                             imd_q5 == "3" ~ df_pop$imd_pop[3],
                             imd_q5 == "4" ~ df_pop$imd_pop[4],
                             imd_q5 == "Least deprived 20%" ~ df_pop$imd_pop[5]))


#exclude dates with cancelled events
excl <- (dt_runs$date %nin% cancelled_events$date)

dt_runs <- dt_runs[excl, ]

#save for analysis
 saveRDS(object = dt_runs,
         file = "data/clean/dt_finisher_ts.rds")



#==========================#
#     ACCESS TO EVENTS     #
#==========================#

# load shape files with locations of events.
lsoa_locations <- shapefile(x = "data/raw/lsoa_centroids/england_lsoa_2011_centroids.shp") # read in shape file
lsoa_locations <- spTransform(x = lsoa_locations, CRSobj = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # use correct projection
lsoa_centr     <- coordinates(obj = lsoa_locations) # extract coordinates
rownames(lsoa_centr) <- lsoa_locations$code

# Want to calculate for each lsoa and month-year what the distance to nearest event was on 15th (middle of month)
distM <- geosphere::distm(x= lsoa_locations,
                         y=cbind(event_locations$lng,
                                 event_locations$lat))
# convert meters to km
distM           <- distM / 1000
dimnames(distM) <- list(rownames(lsoa_centr),
                        event_locations$course) # set row and column names

## loop to assess access in any given month  (month_years)
# (may take a while to run)
months_t <- as.Date(x = paste(unique(runs_full$month_year), "-15", sep="")) # middle of the month

distance.df <- matrix(ncol = 3,
                      nrow = length(months_t) * length(lsoa_locations$name),
                     data = NA)
colnames(distance.df) = c("lsoa","month_year","access")
index = 0
for(t in months_t){
  # create a date
  t = as.Date(t, origin = as.Date("1970-01-01"))
  cat("\r  at:", as.character(t))
  events_in_operation <- (event_locations$Estblsh <= t) # which parkrun events were active in month t?
  temp.dist = get_min_dist(available_event_cols = events_in_operation,
                           month_year = substr(x = t, start = 1, stop = 7),
                           distance_matrix = distM)

  index_rows = 1:length(lsoa_locations$name) + ( index * length(lsoa_locations$name) )
  distance.df[index_rows,] = as.matrix(temp.dist)

  index = index + 1
}


#=========#
# MERGE DATA
#=========#


# # merge all the data-sets except distance to nearest event
 lsoa_df_monthly <- Reduce(function(x, y) merge(x, y, by="lsoa", all=TRUE),
                          list(runs_full, lsoa_imd, lsoa_ethnicity, lsoa_density ))

 # quick check:
 test <- lsoa_df_monthly[,
                   .(finishers = sum(finishers, na.rm = T)),
                   by = c("month_year")]
 plot(as.Date(paste0(test$month_year, "-15")), test$finishers)



# merge distance to nearest event
 lsoa_df_monthly <- merge(x = lsoa_df_monthly,
                          y = distance.df,
                          by= c("lsoa","month_year"),
                          all = TRUE)

 lsoa_df_monthly$access = as.numeric(lsoa_df_monthly$access)

 # quick check:
 test <- lsoa_df_monthly[,
                         .(finishers = sum(finishers, na.rm = T)),
                         by = c("month_year")]
 plot(as.Date(paste0(test$month_year, "-15")), test$finishers)

#=========#
# SAVE DATASETS
#=========#

# save files to cleandata
saveRDS(object = lsoa_df_monthly,
        file = "data/clean/lsoa_df_monthly23.rds")
