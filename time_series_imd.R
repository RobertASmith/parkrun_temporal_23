
library(tidyverse)
library(jtools)
library(data.table)

rm(list = ls())

source("R/get_min_dist.R")
source("R/agg_stats.R")

lsoa_df_monthly <- readRDS("data/clean/lsoa_df_monthly23.rds")


imd_quintiles_cuts <- quantile(x = lsoa_df_monthly$imd_score,
                               probs = seq(0, 1, 0.2), na.rm= T)

lsoa_df_monthly$imd_q5 <- cut(x = lsoa_df_monthly$imd_score,
                              imd_quintiles_cuts,
                              include.lowest = T)

levels(lsoa_df_monthly$imd_q5) <-  c("Least deprived 20%","4", "3", "2", "Most deprived 20%")

lsoa_df_monthly$imd_q5 <- factor(x = lsoa_df_monthly$imd_q5,
                                 levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))

imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[1:length(imd_colors)]


#aggregate by month and by imd quintile
lsoa_df_monthly$finisher_rate <- lsoa_df_monthly$finishers*1000/lsoa_df_monthly$total_pop

df_timeseries <- lsoa_df_monthly |>
  group_by(month_year, imd_q5) |>
  summarise(total_pop = sum(total_pop),
            finishers = sum(finishers))


##wrangle df_timeseries##


df_timeseries <- df_timeseries |>
  filter(is.na(imd_q5) == F)



#add covid periods
#order time-series by date
df_timeseries$date <- parse_date_time(df_timeseries$month_year,
                                orders = "%y-%m")
df_timeseries$date <- trunc(df_timeseries$date, "months")


df_timeseries <- df_timeseries[order(x = df_timeseries$date),]

# Fill in the missing period
all_months <- data.table(date = rep(
                                seq(min(df_timeseries$date),
                                    max(df_timeseries$date),
                                    by = "month"),
                                each = 5))
all_months <- as.data.frame(all_months)


df_timeseries <- merge(
  x = all_months,
  y = df_timeseries,
  by = c("date"),
  all = T
) |>
#remove 2023-03 (no data for this month)
filter(date != "2023-03-01")

# replace NA with 0
df_timeseries$finishers[is.na(df_timeseries$finishers)] <- 0

# define covid period - looking for low numbers of runs at some point...
covid_period <- range(df_timeseries$date[which(x = df_timeseries$finishers < 100)])
covid_period # check, should be around march 20 to july 21. Maybe just remove Mar'20 - Jul'21.

# create a set of variables which define this period
# since start of analysis
df_timeseries$t <- df_timeseries$date - min(df_timeseries$date)
# since lockdown:
df_timeseries$t_since_mar20 <- df_timeseries$date - covid_period[1]
df_timeseries$t_since_mar20[df_timeseries$t_since_mar20 < 1] <- 0
# since reopen
df_timeseries$t_since_jul21 <- df_timeseries$date - covid_period[2]
df_timeseries$t_since_jul21[df_timeseries$t_since_jul21 < 1] <- 0

# create a variable for the covid period
df_timeseries$covid_period <- NA
df_timeseries$covid_period[df_timeseries$date  < covid_period[1]] <- "pre-lockdown"
df_timeseries$covid_period[df_timeseries$date >= covid_period[1] & df_timeseries$date <= covid_period[2]] <- "lockdown"
df_timeseries$covid_period[df_timeseries$date  > covid_period[2]] <- "post-lockdown"
# convert to factor
df_timeseries$covid_period <- as.factor(df_timeseries$covid_period)




n_months <- nrow(df_timeseries)/5

df_timeseries$t <- rep(1:n_months,each=5 )


#visualise trend over time
ggplot(df_timeseries,aes(x=date,
                         y=finishers,
                         col=imd_q5)) +
  ylab(label = "Monthly finishers per 1000 residents")+
  geom_line() +
  #geom_line(alpha=0.7,linewidth=0.3) +
  #geom_smooth(alpha=1,se=F) +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  # ylab("Mean distance to the nearest parkrun event") +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))


#fit simple segmented model with imd as a covariate
mod0 <- glm(data = df_timeseries,
            family = poisson(link = "log"),
            #subset = df_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ poly(t,3) + imd_q5 + covid_period
                                            +covid_period * poly(t,3)
                                            +imd_q5 * covid_period
                                            + offset(log(total_pop)))


jtools::summ(mod0)

df_timeseries$pred <- predict(mod0, df_timeseries, type = "response")

ggplot(data = df_timeseries,
       aes(x = t,
           y = pred,
           col = imd_q5)) +
         geom_line() +
         geom_point(aes(x = t,
                       y = finishers,
                       col = imd_q5))


lines(x = df_timeseries$t,
      y = predicted_runs0, col = "darkgreen")


