rm(list = ls())

# install.packages("zoo")
# install.packages("scales")
# install.packages("ggplot2")

library(zoo)
library(ggplot2)
library(scales)
library(data.table)

# load in the time series data
dt_timeseries <- readRDS(file = "data/clean/dt_finisher_ts.rds")
# order by date
dt_timeseries <- dt_timeseries[order(x = dt_timeseries$date),]

# fill in missing data
all_weeks <- data.table(date = seq(as.Date("2010-01-02"), as.Date("2023-02-25"), 7))
dt_timeseries <- merge(
  x = all_weeks,
  y = dt_timeseries,
  by = c("date"),
  all = T
)
# replace NA with 0
dt_timeseries$finishers[is.na(dt_timeseries$finishers)] <- 0

# create a new column with the rolling mean ...
dt_timeseries$rollmean <- zoo::rollmean(x = dt_timeseries$finishers, k = 24, na.pad = T,)

# Basic visual
plot(x = dt_timeseries$date,
     y = dt_timeseries$finishers,
     type = "p",pch = 20, col = "grey",
     main = "Number of finishers at parkrun by week",
     ylab = "Weekly finishers", xlab = "")
lines(x = dt_timeseries$date,
      y = dt_timeseries$rollmean,
      col = "black")

# define covid period
covid_period <- range(dt_timeseries$date[which(x = dt_timeseries$finishers < 500)])
covid_period # check, should be around march 20 to july 21.
dt_timeseries$covid <- NA
dt_timeseries$covid[dt_timeseries$date < covid_period[1]] <-  "Pre-covid"
dt_timeseries$covid[dt_timeseries$date > covid_period[2]] <-  "Post-covid"

# remove the rolling mean during the covid period
dt_timeseries$rollmean[is.na(dt_timeseries$covid)]  <- NA
dt_timeseries$finishers[is.na(dt_timeseries$covid)] <- NA
# Add in seasons
df_seasons <- data.frame(month  = month.name,
                         season = c(
                           rep("winter", 2),
                           rep("spring", 3),
                           rep("summer", 3),
                           rep("autumn", 3),
                           "winter"
                         ))
dt_timeseries$month <- months(x = dt_timeseries$date)
dt_timeseries <- merge(x = dt_timeseries, y = df_seasons,by = "month")

# plotting data

ggplot() +
  theme_classic() +
  geom_point(data = dt_timeseries,
             mapping = aes(x = date, y = finishers, col = as.factor(season), group = covid))+
  geom_line(data = dt_timeseries,
             mapping = aes(x = date, y = rollmean, group = covid),
              col = "black") +
  geom_smooth(data = dt_timeseries,
              mapping = aes(x = date, y = finishers, col = season, group = covid),
              method = "loess",
              formula = "y ~ x + x^2")+
  scale_x_date(name = "", minor_breaks = "year", expand = c(0, 0))+
  scale_y_continuous(name = "Number of weekly finishers",
                     labels = unit_format(unit = "K", scale = 1e-3),
                     expand = c(0, 0)) +
  scale_colour_viridis_d()
