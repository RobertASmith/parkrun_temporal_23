# R script to clean data
rm(list = ls())

# install.packages("data.table)
# install.packages("curl)

library(data.table)
library(raster)
library(geosphere)

`%nin%` <- Negate(`%in%`)

source("R/get_min_dist.R")

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
event_locations$month_year <- substr(event_locations$Estblsh, 1, 7) # get month and year


#===============================#
#     POPULATION DENSITY        #
#===============================#

lsoa_density <- fread("https://raw.githubusercontent.com/RobertASmith/parkrun_temporal/master/rawdata/Mid-2017%20Population%20Density.csv",
                     stringsAsFactors = F)
lsoa_density <- lsoa_density[grep(pattern = "E",`Code`),
                            .(lsoa = `Code`,
                              pop_density = `People per Sq Km`)]
lsoa_density$pop_density <- as.numeric(gsub(",", "", lsoa_density$pop_density))
# convert ppl / km^2 to 1,000* ppl / km^2
lsoa_density$pop_density <- lsoa_density$pop_density/1000

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

# store the overall time series data in the clean folder.
# saveRDS(object = dt_runs[,
#                          .(finishers = sum(finishers)),
#                          by = c("date")],
#         file = "data/clean/dt_finisher_ts.rds")

# aggregate up by month and year
dt_runs$month_year = substr(x = dt_runs$date, start = 1, stop = 7)
dt_runs <- dt_runs[,
                   .(finishers = sum(finishers)),
                   by = c("month_year","lsoa")]

# create full set of all months and years since the 2011
df_yrs_months <- expand.grid("months" =  c("01","02", "03","04", "05", "06", "07", "08", "09", "10", "11", "12"), "year" = 2011:2023) |> as.data.frame()
df_yrs_months$month_year <- paste0(df_yrs_months$year, "-", df_yrs_months$months)
df_yrs_months <- df_yrs_months[df_yrs_months$month_year %nin% paste0("2023-", c("04", "05", "06", "07", "08", "09", "10", "11", "12")), ]

# merge template fill_dat with runs data.
fill_dat = expand.grid(month_year = unique(df_yrs_months$month_year),
                       lsoa = unique(lsoa_imd$lsoa)) |> as.data.table()

# merge the two datasets, inserting the runs into the lsoa data
runs_full <- merge(x = fill_dat,
                   y = dt_runs,
                   by=c("month_year","lsoa"),
                   all = T)

# filling missing data
runs_full$finishers[is.na(runs_full$finishers)] <- 0

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

# merge distance to nearest event
 lsoa_df_monthly <- merge(x = lsoa_df_monthly,
                          y = distance.df,
                          by= c("lsoa","month_year"))

 lsoa_df_monthly$access = as.numeric(lsoa_df_monthly$access)

#=========#
# SAVE DATASETS
#=========#

# save files to cleandata
saveRDS(object = lsoa_df_monthly,
        file = "data/clean/lsoa_df_monthly23.rds")

#============#
rm(list = ls())

# read data in...
lsoa_df_monthly <- readRDS("data/clean/lsoa_df_monthly23.rds")

imd_quintiles_cuts <- quantile(x = lsoa_df_monthly$imd_score,
                               probs = seq(0, 1, 0.2))

lsoa_df_monthly$imd_q5 <- cut(x = lsoa_df_monthly$imd_score,
                         imd_quintiles_cuts,
                         include.lowest = T)

levels(lsoa_df_monthly$imd_q5) <-  c("Least deprived 20%","4", "3", "2", "Most deprived 20%")

lsoa_df_monthly$imd_q5 <- factor(x = lsoa_df_monthly$imd_q5,
                                 levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))



# aggregate data across year and IMD...
df_agg <- aggregate(
  access ~ imd_q5 + month_year,
  data = lsoa_df_monthly,
  FUN = mean,
  na.rm = TRUE
)



agg_parkrun_stat = function(df, y){

  fig_df = eval(parse(text = paste("aggregate(",y," ~ imd_q5 + month_year, df, mean)")))

  fig_df$plot_date = as.Date(paste(fig_df$month_year,"15",sep="-"))

  fig_general = eval(parse(text = paste("aggregate(",y," ~ month_year, df, mean)")))

  fig_general$plot_date = as.Date(paste(fig_general$month_year,"15",sep="-"))

  fig_general$imd_q5 = "Overall"

  fig_general = fig_general[,names(fig_df)]

  fig_df = rbind(fig_df, fig_general)

  return(fig_df)

}

imd_colors <- c("orangered","orange","yellow3","yellowgreen","lawngreen")
fig1_df <- agg_parkrun_stat(df = lsoa_df_monthly,
                            y="access")

#plot1 <-
ggplot(data = fig1_df,
       mapping = aes(x = plot_date,
                     y = access,
                     col = imd_q5)) +
  geom_point(size=0.2)+
  geom_line() +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  ylab("Mean distance to the nearest parkrun event (km)") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  scale_y_continuous(trans = scales::log2_trans()) +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))

