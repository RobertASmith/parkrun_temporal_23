rm(list = ls())
library(tidyverse)
library(jtools)
library(data.table)
library(splines)
library(zoo)


source("R/get_min_dist.R")
source("R/agg_stats.R")




df_timeseries <- readRDS(file = "data/clean/dt_finisher_ts.rds")
df_events <- readRDS(file = "data/clean/n_events.rds")


imd_levels <- c("Least deprived 20%","4", "3", "2", "Most deprived 20%")
imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[1:length(imd_colors)]



# replace NA with 0
df_timeseries$finishers[is.na(df_timeseries$finishers)] <- 0

#define covid  period
covid_period <- range(ymd("2020-03-01"), ymd("2021-07-17"))


# create a variable for the covid period
df_timeseries$covid_period <- NA
df_timeseries$covid_period[df_timeseries$date  < covid_period[1]] <- "pre-lockdown"
df_timeseries$covid_period[df_timeseries$date >= covid_period[1] & df_timeseries$date <= covid_period[2]] <- "lockdown"
df_timeseries$covid_period[df_timeseries$date  > covid_period[2]] <- "post-lockdown"
# convert to factor
df_timeseries$covid_period <- as.factor(df_timeseries$covid_period)
#create separate covid and post covid variables for modelling
df_timeseries$covid <- ifelse(df_timeseries$covid_period == "lockdown", 1,0)
df_timeseries$post_covid <- ifelse(df_timeseries$covid_period == "post-lockdown", 1,0)
#create variable 't' (time since start of study period)
n_weeks <- nrow(df_timeseries)/5
n_four_week <- nrow(df_timeseries) / 4
df_timeseries$t <- rep(1:n_weeks,each = 5 )

#add month of the year column to allow for seasonality adjustment
n_weeks = as.numeric(range(df_timeseries$t)[2])

dt_fweeks <- data.table(t = 1:n_weeks,
           four_week = as.factor(rep(1:13, each = 4, times = n_weeks/(13*4))))


df_timeseries <- left_join(x = df_timeseries,
          y = dt_fweeks,
          by = "t")

#set reference level to pre-lockdown
df_timeseries$covid_period <- relevel(df_timeseries$covid_period,
                                      ref = "pre-lockdown")
#add rate column (finishers/1000pop)
df_timeseries$rate <- df_timeseries$finishers *1000/df_timeseries$total_pop

##Discard pre-2015 data: trend changed over time
df_timeseries <- df_timeseries[df_timeseries$date >"2016-01-01"]

#visualise trend over time
plot0 <- ggplot(df_timeseries[df_timeseries$covid_period == "pre-lockdown",],
       ) +
  ylab(label = "Monthly finishers per 1000 residents")+
  xlab(label = "Date") +
  geom_line(linewidth = 0.3, aes(x = date, y = rate, col = imd_q5)) +
  #scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))

plot0


plot1 <- plot0 +
  geom_smooth( se = F, method = "glm", formula = y ~ log(x),
               aes(x = date, y = rate, col = imd_q5)) +
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
              aes(x = date, y = rate, col = imd_q5),
            linewidth = 0.3) +
  geom_smooth(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
              aes(x = date, y = rate, col = imd_q5),
              se = F,
              method = "glm", formula = y ~ poly(x,1))
plot1

ggsave(plot = plot1,width = 10, height=6,
       filename = "outputs/Figure1.png")

#add weekly number of events to plot
df_events <- df_events |>
  distinct(eventname, date) |>
  group_by(date) |>
  summarise(n_event = n())


  ggplot() +
  geom_point(data = df_events, aes(x = date, y = n_event))



####decompose timeseries to ID seasonality####

ts2 <- ts(df_timeseries$finishers, start = c(2015, 1), end = c(2023, 7), frequency = 52)
plot.ts(ts2)


decomp <- function(imd) {
timeseries_imd <- df_timeseries[df_timeseries$imd_q5 == imd]
timeseries_imd <- timeseries_imd[timeseries_imd$covid_period == "pre-lockdown"]
timeseries_imd <- timeseries_imd[timeseries_imd$date >"2015-01-01"]
ts2 <- ts(timeseries_imd$finishers, start = c(2015, 1), end = c(2020, 6), frequency = 52)
de2 <- decompose(ts2, type = "multiplicative")
plot(de2)
return(de2)}

de_most_dep <- decomp(imd = "Most deprived 20%")

de_most_dep$seasonal

####fit models for each deprivation quintile separately for ease of interpretation ####

fit_glm <- function (df = df_timeseries, imd) {


  df <- df |>
    filter(imd_q5 == {imd})

spl <- bs(df$t, degree = 3, df = 60)

 mod <- glm(data = df,
            family = poisson(link = "log"),
            formula = finishers ~ log(t)  * covid_period
            +covid_period
            #+ covid * t
            #+ spl
            +four_week * t
            + offset(log(total_pop)))


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


#plot model as check
plot0
  ggplot(data = preds,
       aes(x = date, y = rate, col = imd)) +
  geom_line(linetype = "dashed")




##plot extrapolated trends

fit_glm_ex <- function (df = df_timeseries, imd) {


  df <- df |>
    filter(imd_q5 == {imd})

#sp <- bs(df$t, degree = 3, df = 60)

  mod <- df |>
    filter(covid_period == "pre-lockdown") |>
    glm(family = quasipoisson(link = "log"),
             formula = finishers ~ log(t)  * four_week
             +offset(log(total_pop)))


  print(jtools::summ(mod))

  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd = {imd},
                   covid_period = df$covid_period,
                   date = df$date)

  dt$rate =  dt$finishers *1000/df$total_pop

 # plot(mod)


  return(dt)
}

preds_ex <- bind_rows(fit_glm_ex(df_timeseries, "Most deprived 20%"),
                   fit_glm_ex(df_timeseries, "2"),
                   fit_glm_ex(df_timeseries, "3"),
                   fit_glm_ex(df_timeseries, "4"),
                   fit_glm_ex(df_timeseries, "Least deprived 20%"))




plot0a <- ggplot(data = df_timeseries[df_timeseries$covid_period == "pre-lockdown",],
                aes(y = rate, x = date,
                col = imd_q5)) +
  geom_line()

plot0a

plot2 <-  plot1 + geom_smooth(data = preds_ex,
                       aes(x = date, y = rate, col = imd),
                       method = "glm",
                       formula = y ~ log(x),
                       linetype = "solid",
                       linewidth = 0.5,
                       se = F) +
              geom_line(data = preds_ex
              [preds_ex$covid_period == "post-lockdown"|
                        preds_ex$covid_period == "lockdown",]
              ,
              aes(x = date, y = rate, col = imd),
              linetype = "solid",
              linewidth = 0.5)
plot2


ggsave(plot = plot2,width = 10, height=6,
       filename = "outputs/Figure2.png")

#overall model function
mod_func <- function(p){
mod_all <- glm(data = df_timeseries,
                      family = poisson(link = "log"),
                      formula = finishers ~ poly(t,p) * covid_period
                      + covid_period
                      + t *four_week
                      + imd_q5
                      +offset(log(total_pop)))
sum_all <- summ(mod_all)
print(sum_all)
return(mod_all)}

mod_1 <- mod_func(p=1)
mod_2 <- mod_func(p=2)
mod_3 <- mod_func(p=3)
mod_4 <- mod_func(p=4)
mod_5 <- mod_func(p=5)

anova(mod_4,mod_5, test = "LRT")


pred_all <- data.table(
                 finishers = predict(mod_3, df_timeseries, type = "response"),
                 imd = df_timeseries$imd_q5,
                 month = df_timeseries$month_seas,
                 covid_period = df_timeseries$covid_period,
                 date = df_timeseries$date)

pred_all$rate =  pred_all$finishers *1000/df_timeseries$total_pop



ggplot(data = pred_all,
       aes(x = date, y = rate, col = imd)) +
  geom_line()




# mod_s <- glm(data = df_timeseries,
#                family = poisson(link = "log"),
#                subset = df_timeseries$covid_period == "pre-lockdown",
#                formula = finishers ~ 1/(1+exp(-t))
#                # * covid_period
#                + t * four_week
#                + imd_q5
#                +offset(log(total_pop)))
#  summ(mod_s)
#
#
#  pred_s <- data.table(
#    finishers = predict(mod_s, df_timeseries, type = "response"),
#    imd = df_timeseries$imd_q5,
#    covid_period = df_timeseries$covid_period,
#    date = df_timeseries$date)
#
#  pred_s$rate =  pred_s$finishers *1000/df_timeseries$total_pop
#
#
#
#  ggplot(data = pred_s,
#         aes(x = date, y = rate, col = imd)) +
#    geom_line()


####calculate number of participants lower than expected####
#subset post covid for predicted and actual datasets
preds_pc <- preds_ex[covid_period == "post-lockdown"]
obs_pc <- df_timeseries[covid_period == "post-lockdown"]
obs_pc$imd <- obs_pc$imd_q5


df_lost <- merge(preds_pc,
                 obs_pc,
                 by = c("date", "imd"))


df_lost <- data.table(date = df_lost$date,
                      imd = df_lost$imd,
                      predicted  = df_lost$finishers.x,
                      observed = df_lost$finishers.y,
                      lost = df_lost$finishers.x - df_lost$finishers.y)

#sum lost finishers by imd
df_lost |>
  group_by(imd) |>
  summarise(predicted = sum(predicted),
            observed = sum(observed),
            lost = sum(lost)) |>
  mutate(total = sum(lost))

#plot lost participants over time
ggplot(data = df_lost,
       aes(x = date, y = lost, col = imd)) +
  geom_line()


#how many participants fewer on most recent date 2023-02-25
df_lost |>
  filter(date == "2023-01-28") |>
  select(-date) |>
  mutate(total_pred = sum(predicted))



