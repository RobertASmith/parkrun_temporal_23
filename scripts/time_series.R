rm(list = ls())

# load necessary packages
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
library(lubridate)

# load in the time series data
dt_timeseries <- readRDS(file = "data/clean/dt_finisher_ts.rds")

# order time-series by date
dt_timeseries <- dt_timeseries[order(x = dt_timeseries$date),]

# Fill in the missing period
all_weeks <- data.table(date = seq(min(dt_timeseries$date),
                                   max(dt_timeseries$date), 7))
dt_timeseries <- merge(
  x = all_weeks,
  y = dt_timeseries,
  by = c("date"),
  all = T
)

# replace NA with 0
dt_timeseries$finishers[is.na(dt_timeseries$finishers)] <- 0

# define covid period - looking for low numbers of runs at some point...
covid_period <- range(dt_timeseries$date[which(x = dt_timeseries$finishers < 500)])
covid_period # check, should be around march 20 to july 21. Maybe just remove Mar'20 - Jul'21.

# create a set of variables which define this period
# since start of analysis
dt_timeseries$t <- dt_timeseries$date - min(dt_timeseries$date)
# since lockdown:
dt_timeseries$t_since_mar20 <- dt_timeseries$date - covid_period[1]
dt_timeseries$t_since_mar20[dt_timeseries$t_since_mar20 < 1] <- 0
# since reopen
dt_timeseries$t_since_jul21 <- dt_timeseries$date - covid_period[2]
dt_timeseries$t_since_jul21[dt_timeseries$t_since_jul21 < 1] <- 0

# create a variable for the covid period
dt_timeseries$covid_period <- NA
dt_timeseries$covid_period[dt_timeseries$date  < covid_period[1]] <- "pre-lockdown"
dt_timeseries$covid_period[dt_timeseries$date >= covid_period[1] & dt_timeseries$date <= covid_period[2]] <- "lockdown"
dt_timeseries$covid_period[dt_timeseries$date  > covid_period[2]] <- "post-lockdown"
# convert to factor
dt_timeseries$covid_period <- as.factor(dt_timeseries$covid_period)
# create a variable for week of the year
dt_timeseries$week <- factor(x = week(dt_timeseries$date), levels = 1:53)
dt_timeseries$week[dt_timeseries$week == 53] <- 52



# create a GLM model
mod0 <- glm(data = dt_timeseries,
            subset = dt_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ poly(t, 3))#+ poly(t_since_mar20, 3) + poly(t_since_jul21, 3))

mod0a <- glm(data = dt_timeseries,
            formula = finishers ~ poly(t, 3) * covid_period)#+ poly(t_since_mar20, 3) + poly(t_since_jul21, 3))

mod1 <- glm(data = dt_timeseries,
            formula = finishers ~ poly(t, 3) * covid_period * week)#+ poly(t_since_mar20, 3) + poly(t_since_jul21, 3))
# model 2 trained on only pre covid data:
mod2 <- glm(data = dt_timeseries,
            subset = dt_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ poly(t, 3) * week)#+ poly(t_since_mar20, 3) + poly(t_since_jul21, 3))



predicted_runs0 <- predict(object = mod0,
                           newdata = data.frame(t = as.numeric(dt_timeseries$t)))

predicted_runs0a <- predict(object = mod0a,
                           newdata = data.frame(t = as.numeric(dt_timeseries$t),
                                                covid_period = dt_timeseries$covid_period))

predicted_runs1 <- predict(object = mod1,
                         newdata = data.frame(t = as.numeric(dt_timeseries$t),
                                              covid_period = dt_timeseries$covid_period,
                                              week = dt_timeseries$week))

predicted_runs2 <- predict(object = mod2,
                          newdata = data.frame(t = as.numeric(dt_timeseries$t),
                                               week = dt_timeseries$week))

plot(x = dt_timeseries$date,
     y = dt_timeseries$finishers/1000,
     type = "l",
     ylim = c(0, 250),
     xlab = "",
     ylab = "Number of finishers (thousands per week)")
lines(x = dt_timeseries$date,
      y = predicted_runs0/1000, col = "darkgreen")
lines(x = dt_timeseries$date,
      y = predicted_runs0a/1000, col = "blue")
lines(x = dt_timeseries$date,
      y = predicted_runs1/1000, col = "blue")
lines(x = dt_timeseries$date,
      y = predicted_runs2/1000, col = "red")


v_parkrun_covid_effect <- (predicted_runs2 - dt_timeseries$finishers)
sum(v_parkrun_covid_effect) / 1e+06
sum(v_parkrun_covid_effect[dt_timeseries$covid_period != "pre-lockdown"]) / 1e+06

plot(x = dt_timeseries$date,
     y = v_parkrun_covid_effect/1000,
     type = "l",
     ylim = c(0, 250),
     xlab = "",
     ylab = "Number of finishers (thousands per week)")














# convert to timeseries format:
ts_finishers <- ts(dt_timeseries$finishers,
                   freq = 365 / 7,
                   start = 0)#decimal_date(ymd(min(dt_timeseries$date))))

# decompose the timeseries into:
# seasonal variation, trend, random noise
de_finishers <- decompose(x = ts_finishers, type = "multiplicative")
plot(de_finishers)
HoltWinters(x = ts_finishers)




ggplot(data = dt_timeseries,
       mapping = aes(x = date, y = finishers, group = covid_period)) +
 theme_classic() +
 geom_point()+
 geom_smooth(method = "glm", formula = y ~ poly(x,3), )





dt_timeseries$covid <- NA
dt_timeseries$covid[dt_timeseries$date < covid_period[1]] <-  "Pre-covid"
dt_timeseries$covid[dt_timeseries$date > covid_period[2]] <-  "Post-covid"
dt_timeseries$covid[dt_timeseries$date >= covid_period[1] & dt_timeseries$date <= covid_period[2]] <- "During_covid"



# create a new column with the rolling mean ...
#dt_timeseries$rollmean <- zoo::rollmean(x = dt_timeseries$finishers,
#                                        k = 24,
#                                        na.pad = T)
#
## Basic visual
#plot(x = dt_timeseries$date,
#     y = dt_timeseries$finishers,
#     type = "p",pch = 20, col = "grey",
#     main = "Number of finishers at parkrun by week",
#     ylab = "Weekly finishers", xlab = "")
#lines(x = dt_timeseries$date,
#      y = dt_timeseries$rollmean,
#      col = "black")






# remove the rolling mean during the covid period
#dt_timeseries$rollmean[is.na(dt_timeseries$covid)]  <- NA
#dt_timeseries$finishers[is.na(dt_timeseries$covid)] <- NA


# Add in seasons
#df_seasons <- data.frame(month  = month.name,
#                         season = c(
#                           rep("winter", 2),
#                           rep("spring", 3),
#                           rep("summer", 3),
#                           rep("autumn", 3),
#                           "winter"
#                         ))
#dt_timeseries$month <- months(x = dt_timeseries$date)
#dt_timeseries <- merge(x = dt_timeseries, y = df_seasons,by = "month")
#
## plotting data
#
,

# fill in missing data

#                           y = log(finishers),
#                           col = as.factor(season),
#                           group = covid))+
#  #geom_line(data = dt_timeseries,
#  #           mapping = aes(x = date,
#  #                         y = rollmean,
#  #                         group = covid),
#  #            col = "black") +
#  geom_smooth(data = dt_timeseries,
#              mapping = aes(x = date,
#                            y = log(finishers),
#                            col = season,
#                            group = covid),
#              method = "loess",
#              formula = "y ~ x + x^2")+
#  scale_x_date(name = "", minor_breaks = "year", expand = c(0, 0))+
#  scale_y_continuous(name = "Number of weekly finishers",
#                     labels = unit_format(unit = "K", scale = 1e-3),
#                     expand = c(0, 0)) +
#  scale_colour_viridis_d()


# split data into pre and post covid:
dt_pre <- dt_timeseries |> dplyr::filter(covid == "Pre-covid")
dt_post <- dt_timeseries |> dplyr::filter(covid == "Post-covid")


ts_pre <- ts(dt_pre$finishers,
   freq=365/7,
   start=decimal_date(ymd(min(dt_pre$date))))

ts_post <- ts(dt_post$finishers,
             freq=365/7,
             start=decimal_date(ymd(min(dt_post$date))))



#timeseries <- timeseries[!is.na(x = timeseries)]

plot(ts_pre,
     type='l',
     col="blue", xlab="Month", ylab="Runs")

plot(ts_post,
     type='l',
     col="blue", xlab="Month", ylab="Runs")


df_decom_pre <- decompose(x = (ts_pre), type = "multiplicative")
df_decom_post <- decompose(x = (ts_post), filter = NULL,)


dt_pre$deseasonalised = df_decom_pre$x - df_decom_pre$seasonal

HoltWinters(x = dt_pre$finishers)


hist(df_decom_pre$random, breaks = 100)

dt_timeseries$date[df_decomposed$random < -30000]
hist(df_decomposed$random,breaks = seq(-80000, 80000, 1000))
