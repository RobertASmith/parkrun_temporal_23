# R script to clean data

rm(list = ls())

# Read in all csv files in event-data
participation_data_folder <- "data/eventdata"

# get a vector of all of the data files in the folder
v_parkrun_runs_csvs <- list.files(path = participation_data_folder)

# function to read in a specific file.
get_participation_csv <- function(file_path) {
  df <- read.csv(paste0(participation_data_folder, "/", file_path))
  return(df)
}

# read in the files and store in a list.
l_part_data <- lapply(X = v_parkrun_runs_csvs,
                               FUN = get_participation_csv)

# unlist the data
df_participation <- do.call(rbind,l_part_data)










runs_df = readRDS("rawdata/runs_per_lsoa_2010to2020.Rds")
runs_df = runs_df[grep(pattern = "E",lsoa)] # select LSOAs in England only
runs_df$month_year = substr(runs_df$date,1,7) # create a month & year variable
runs_df$date = as.Date(unclass(runs_df$date),format="%Y-%m-%d") # convert to Date class
runs_df = runs_df[weekdays(runs_df$date) == "Saturday"] # restrict to saturday events only
runs_df = runs_df[runs_df$date <= as.Date("2019-12-31"),] # restrict to events before 2019
runs_df = runs_df[,.(finishers = sum(finishers)),by = c("month_year","lsoa")]

# merge template fill_dat with runs data.
fill_dat = expand.grid(month_year = unique(runs_df$month_year),
                       lsoa = unique(lsoa_imd$lsoa)) %>% data.table # using all English LSOAs from the imd data set
runs_full = merge(x = fill_dat,
                  y = runs_df,
                  by=c("month_year","lsoa"),
                  all.x=T)

runs_full$finishers[is.na(runs_full$finishers)] = 0  # filling missing data
