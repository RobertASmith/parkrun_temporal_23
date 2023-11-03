rm(list = ls())
library(tidyverse)
library(jtools)
library(data.table)
library(splines)
library(zoo)
library(broom)


source("R/get_min_dist.R")
source("R/agg_stats.R")



df_timeseries <- readRDS(file = "data/clean/dt_finisher_ts.rds")
df_events <- readRDS(file = "data/clean/n_events.rds")

imd_levels <- c("Least deprived 20%","4", "3", "2", "Most deprived 20%")
imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[1:length(imd_colors)]
df_timeseries$imd_q5 <- factor(df_timeseries$imd_q5,
                            levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))

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

##Discard pre-2015 data: trend changed over time
df_timeseries <- df_timeseries[df_timeseries$date >"2015-01-01"]

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

#define t2 as time since intervention (i.e. t since post-lockdown)
df_timeseries$t_since_jul21 <- difftime(df_timeseries$date,
                                        covid_period[2],
                                        units = c("weeks"))
df_timeseries$t_since_jul21[df_timeseries$t_since_jul21 < 1] <- 0
#convert to numeric
df_timeseries$t_since_jul21 <- as.numeric(df_timeseries$t_since_jul21)


#set reference level to pre-lockdown
df_timeseries$covid_period <- relevel(df_timeseries$covid_period,
                                      ref = "pre-lockdown")
#add rate column (finishers/1000pop)
df_timeseries$rate <- df_timeseries$finishers *1000/df_timeseries$total_pop


#visualise trend over time
plot0 <- ggplot(df_timeseries[df_timeseries$covid_period == "pre-lockdown",],
       ) +
  ylab(label = "Monthly finishers per 1000 residents")+
  xlab(label = "Date") +
  geom_line(linewidth = 0.3, aes(x = date, y = rate, col = imd_q5)) +
  scale_color_manual(values=c(imd_colors),name="IMD quintile") +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.9),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))

plot0


plot1 <- plot0 +
  geom_smooth( se = F, method = "glm",
               method.args = list(family = "poisson"),
               formula = y ~ poly(x,2),
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
       filename = "outputs/Figure1_participation.png")

#plot weekly number of events
df_events <- df_events |>
  distinct(eventname, date) |>
  group_by(date) |>
  summarise(n_event = n())


  plot2 <- ggplot(data = df_events, aes(x = date, y = n_event)) +
  geom_point()

  ggsave(plot = plot2,width = 10, height=6,
         filename = "outputs/Figure2_events.png")

####decompose timeseries to ID seasonality####

ts2 <- ts(df_timeseries$finishers, start = c(2015, 1), end = c(2023, 7), frequency = 52)


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
#spl <- bs(df$t, degree = 3, df = 60)

 df <- df |>
       filter(imd_q5 == {imd})
   #    filter(covid_period != "lockdown")

 #df$t <- seq(seq(1: nrow(df)))

 mod <- glm(data = df,
            family = poisson(link = "log"),
            formula = finishers ~ offset(log(total_pop)) +
            poly(t,2) + four_week + covid_period +
            post_covid:poly(t_since_jul21,2)
            )
           # * covid_period *four_week
            #+covid_period
            #+ covid * t
            #+ spl



 mod_sum <- jtools::summ(mod, exp = T) |>
   print()



  return(mod)
}

pred_func <- function(df = df_timeseries, imd) {

  df <- df |>
    filter(imd_q5 == {imd})
   # filter(covid_period != "lockdown")

  #df$t <- seq(seq(1: nrow(df)))

  mod <- glm(data = df,
             family = poisson(link = "log"),
             formula = finishers ~ poly(t,1) * covid_period
             + four_week
             # * covid_period *four_week
             #+covid_period
             #+ covid * t
             #+ spl
             + offset(log(total_pop)))

  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd = {imd},
                   four_week = df$four_week,
                   covid_period = df$covid_period,
                   date = df$date)

  dt$rate =  dt$finishers *1000/df$total_pop
  return(dt)
}

coef_table <- function(mod, imd) {

  mod_sum <- jtools::summ(mod, exp = T)

  mod_sum <- broom::tidy(mod_sum) |>
    filter(term == "covid_periodpost-lockdown"|
           term == "log(t):covid_periodpost-lockdown") |>
    select(c("term", "estimate", "p.value"))

  mod_sum <- data.table(
             imd = {imd},
             term = mod_sum$term,
             estimate = mod_sum$estimate,
             p.value = mod_sum$p.value)

  return(mod_sum)

}


mod1 <- fit_glm(df_timeseries, "3")



bind_rows(coef_table(fit_glm(df_timeseries, "Most deprived 20%"), imd = "Most deprived 20%"),
          coef_table(fit_glm(df_timeseries, "2"), imd = "2"),
          coef_table(fit_glm(df_timeseries, "3"), imd = "3"),
          coef_table(fit_glm(df_timeseries, "4"), imd = "4"),
          coef_table(fit_glm(df_timeseries, "Least deprived 20%"), imd = "Least deprived 20%"))



 preds <- bind_rows(pred_func(df_timeseries, "Most deprived 20%"),
                    pred_func(df_timeseries, "2"),
                    pred_func(df_timeseries, "3"),
                    pred_func(df_timeseries, "4"),
                    pred_func(df_timeseries, "Least deprived 20%"))


#plot model as check
# plot0
   ggplot(data = preds,
        aes(x = date, y = rate, col = imd)) +
   geom_line(linetype = "dashed")




##plot extrapolated trends

fit_glm_ex <- function (df = df_timeseries, imd) {


  df <- df |>
    filter(imd_q5 == {imd})

sp <- bs(df$t, degree = 3, df = 8)

  mod <- df |>
    filter(covid_period == "pre-lockdown") |>
    glm(family = poisson(link = "log"),
        formula = finishers ~ offset(log(total_pop))
                  + poly(t,2) + four_week)

  print(jtools::summ(mod, exp = F))

  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd = {imd},
                   four_week = df$four_week,
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



plot3 <-  plot0 + geom_smooth(data = preds_ex,
                       aes(x = date, y = rate, col = imd),
                       method = "glm",
                       #method.args = list(family = "poisson"),
                       formula = y ~ poly(x,2),
                       linetype = "dashed",
                       linewidth = 0.5,
                       se = F) +
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
            aes(x = date, y = rate, col = imd_q5),
            linewidth = 0.3) +
  geom_smooth(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
              aes(x = date, y = rate, col = imd_q5),
              se = F,
              method = "glm", formula = y ~ poly(x,1))
              #geom_line(data = preds_ex
              #[preds_ex$covid_period == "post-lockdown"|
              #          preds_ex$covid_period == "lockdown",]
              #,
              #aes(x = date, y = rate, col = imd),
              #linetype = "solid",
              #linewidth = 0.5)
plot3
ggsave(plot = plot3,width = 10, height=6,
       filename = "outputs/Figure3_preds.png")


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

#predicted-observed / predicted
df_lost |>
  distinct(imd, .keep_all = T) |>
  mutate(prop_decrease = lost/predicted)

#plot lost participants over time
ggplot(data = df_lost,
       aes(x = date, y = lost, col = imd)) +
  geom_line()

#how many participants fewer on most recent date 2023-02-25
df_lost |>
  filter(date == "2023-01-28") |>
  select(-date) |>
  mutate(total_pred = sum(predicted))
