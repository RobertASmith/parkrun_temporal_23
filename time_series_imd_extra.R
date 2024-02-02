
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



df_timeseries <- df_timeseries |>
#remove 2023-03 (no data for this month)
filter(date != "2023-03-01")

df_timeseries$date <- as.POSIXct(df_timeseries$date,
                                 format = "%y-%m-$d")

# replace NA with 0
df_timeseries$finishers[is.na(df_timeseries$finishers)] <- 0

covid_period <- range(ymd("2020-03-01"), ymd("2021-07-01"))
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

df_timeseries$t <- rep(1:n_months,each = 5 )

#add month of the year column to allow for seasonality adjustment
df_timeseries$month_seas <- month(ymd(df_timeseries$date))
#set reference level to pre-lockdown
df_timeseries$covid_period <- relevel(df_timeseries$covid_period,
                                      ref = "pre-lockdown")
#add rate column (finishers/1000pop)
df_timeseries$rate <- df_timeseries$finishers *1000/df_timeseries$total_pop



#visualise trend over time
plot0 <- ggplot(df_timeseries[df_timeseries$covid_period == "pre-lockdown",],
       aes(x = date, y = rate, col = imd_q5)) +
  ylab(label = "Monthly finishers per 1000 residents")+
  xlab(label = "Date") +
  geom_line(linewidth = 0.3) +
  geom_smooth( se = F) +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))



plot1 <- plot0 +
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
              aes(x = date, y = rate, col = imd_q5),
            linewidth = 0.3) +
  geom_smooth(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
              aes(x = date, y = rate, col = imd_q5),
              method = "lm",
              se = F)




ggsave(plot = plot1,width = 10, height=6,
       filename = "outputs/Figure1_access.png")

#comparison of general model and one stratified by imd
mod_gen <- glm(data = df_timeseries,
                       family = poisson(link = "log"),
                       formula = finishers ~ poly(t,3) + imd_q5 + covid_period
                       +covid_period * poly(t,3)
                       + offset(log(total_pop)))



mod_imd <- glm(data = df_timeseries,
                       family = poisson(link = "log"),
                       formula = finishers ~ poly(t,3) + imd_q5 + covid_period
                       +covid_period * poly(t,3)
                       +imd_q5 * covid_period
                       + offset(log(total_pop)))

summ(mod_gen)
summ(mod_imd)

#fit simple segmented model with imd as a covariate
mod0 <- glm(data = df_timeseries,
            family = poisson(link = "log"),
            #subset = df_timeseries$covid_period == "pre-lockdown",
            formula = rate ~ poly(t,3) + imd_q5 + covid_period
                                            +covid_period * poly(t,3)
                                            + imd_q5 * covid_period)

#segmented model accounting for seasonality (month of the year)
mod0a <- glm(data = df_timeseries,
            family = poisson(link = "log"),
            #subset = df_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ poly(t,3) + imd_q5 + covid_period
            + covid_period * poly(t,3)
            + imd_q5 * covid_period
            + as.factor(month_seas)
            + offset(log(total_pop)))


#fit model based on pre-covid trends
mod1 <- glm(data = df_timeseries,
            family = poisson(link = "log"),
            subset = df_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ poly(t,3) + imd_q5
            + offset(log(total_pop)))


####fit models for each deprivation quintile separately for ease of interpretation ####

fit_glm <- function (df = df_timeseries, imd) {


  df <- df |>
    filter(imd_q5 == {imd})

 mod <- glm(data = df,
            family = poisson(link = "log"),
            formula = finishers ~ poly(t,3)
            + covid_period
            + covid_period * poly(t,3)
            + as.factor(month_seas)
            +offset(log(total_pop)))


  print(jtools::summ(mod))

  dt <- data.table(t = seq(1:(nrow(df))),
             finishers = predict(mod, df, type = "response"),
             imd = {imd},
             month = df$month_seas,
             covid_period = df$covid_period,
             date = df$date)


  dt$rate =  dt$finishers *1000/df$total_pop
  return(dt)
}



preds <- bind_rows(fit_glm(df_timeseries, "Most deprived 20%"),
                   fit_glm(df_timeseries, "2"),
                   fit_glm(df_timeseries, "3"),
                   fit_glm(df_timeseries, "4"),
                   fit_glm(df_timeseries, "Least deprived 20%"))





##plot extrapolated trends

fit_glm_ex <- function (df = df_timeseries, imd) {


  df <- df |>
    filter(imd_q5 == {imd})

  mod <- df |>
    filter(covid_period == "pre-lockdown") |>
    glm(
             family = poisson(link = "log"),
            # subset = df$covid_period == "pre-lockdown",
             formula = finishers ~ poly(t,3)
             + as.factor(month_seas)
             +offset(log(total_pop)))


  print(jtools::summ(mod))

  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd = {imd},
                   month = df$month_seas,
                   covid_period = df$covid_period,
                   date = df$date)

  dt$rate =  dt$finishers *1000/df$total_pop


  return(dt)
}

preds_ex <- bind_rows(fit_glm_ex(df_timeseries, "Most deprived 20%"),
                   fit_glm_ex(df_timeseries, "2"),
                   fit_glm_ex(df_timeseries, "3"),
                   fit_glm_ex(df_timeseries, "4"),
                   fit_glm_ex(df_timeseries, "Least deprived 20%"))



plot3 <- plot0 + geom_smooth(data = preds_ex,
                       aes(x = date, y = rate, col = imd),
                       linetype = "dashed",
                       linewidth = 1,
                       se = F) +
  geom_line(data = preds_ex[preds_ex$covid_period == "post-lockdown"|preds_ex$covid_period == "lockdown",],
              aes(x = date, y = rate, col = imd),
              linetype = "dotted",
              linewidth = 0.5,
              se = F)



ggplot(data = preds[preds$covid_period == "pre-lockdown"],
       aes(x = date,
           y = finishers
          # ,col = imd
           )) +
         geom_line() +
  geom_line(data = preds[preds$covid_period == "post-lockdown"],
                  aes(x = date,
                  y = finishers
                  #,col = imd
           )) +
  facet_wrap(~imd, ncol = 2)



pred_plot + vis_plot






