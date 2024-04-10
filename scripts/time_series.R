rm(list = ls())
#load packages - ensure
library(tidyverse)
library(data.table)
library(broom)

#read in cleaned datasets
df_timeseries <- readRDS(file = "data/clean/dt_finisher_ts.rds")
####edit df_timeseries: add columns for modelling and visualisation and set factor levels####
#set IMD levels and colours
imd_levels <- c("Least deprived 20%","4", "3", "2", "Most deprived 20%")
imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[1:length(imd_colors)]

#add number of events as empty level solely for visualisation - to be dropped later
df_timeseries$imd_q5 <- factor(df_timeseries$imd_q5,
                            levels = rev(c("Number of events", "Least deprived 20%","4", "3", "2", "Most deprived 20%")))
#set colour of  number of events to black
key_colors <- append(imd_colors, "black")
key_colors = key_colors[1:length(key_colors)]

# replace NAs with 0 finishers
df_timeseries$finishers[is.na(df_timeseries$finishers)] <- 0

#replace NAs with 0 events
df_timeseries$n_events[is.na(df_timeseries$n_events)] <- 0

#define covid  period based on n events per week
covid_period <- range(df_timeseries$date[which(x = df_timeseries$n_events< 50)])
#check - should be roughly march 2020 - JUly 2021
covid_period

# create a variable for the covid period
df_timeseries$covid_period <- NA
df_timeseries$covid_period[df_timeseries$date  < covid_period[1]] <- "pre-lockdown"
df_timeseries$covid_period[df_timeseries$date >= covid_period[1] & df_timeseries$date <= covid_period[2]] <- "lockdown"
df_timeseries$covid_period[df_timeseries$date  > covid_period[2]] <- "post-lockdown"
# convert to factor
df_timeseries$covid_period <- as.factor(df_timeseries$covid_period)

#create dummy variables for pandemic and post pandemic for modelling
df_timeseries$covid <- ifelse(df_timeseries$covid_period == "lockdown", 1,0)
df_timeseries$post_covid <- ifelse(df_timeseries$covid_period == "post-lockdown", 1,0)

#create variable 't' (time since start of study period)
n_weeks <- (nrow(df_timeseries)/5) -1
n_four_week <- nrow(df_timeseries) / 4
df_timeseries$t <- rep(0:n_weeks,each = 5 )

#add column to represent the year split into 13 4-week periods to allow for seasonality adjustment
#extract number of weeks
n_weeks = as.numeric(range(df_timeseries$t)[2])
#repeat 1:13 to cover entire study period - each level repeated 4 times for the four weeks
dt_fweeks <- data.table(t = 0:n_weeks,
           four_week = as.factor(rep(1:13, each = 4, times = n_weeks/(13*4))))
#left-join with df_timeseries to account for
#the five-rows for each week (one for each IMD quintile)
df_timeseries <- left_join(x = df_timeseries,
          y = dt_fweeks,
          by = "t")

#define t_since_jul21 as time since intervention (i.e. t since post-lockdown)
df_timeseries$t_since_jul21 <- difftime(df_timeseries$date,
                                        covid_period[2],
                                        units = c("weeks"))
df_timeseries$t_since_jul21[df_timeseries$t_since_jul21 < 1] <- 0
#convert to numeric
df_timeseries$t_since_jul21 <- as.numeric(df_timeseries$t_since_jul21)


#add rate column (finishers/1000pop) for visualisation
df_timeseries$rate <- df_timeseries$finishers *1000/df_timeseries$total_pop


####visualise trend over time####



plot0 <- ggplot(df_timeseries[df_timeseries$covid_period == "pre-lockdown",],
       ) +
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
                          aes(x = date, y = rate, col = imd_q5),
                        linewidth = 0.5) +
  ylab(label = "Weekly finishers per 1000 residents")+
  xlab(label = "Date") +
  geom_line(linewidth = 0.5, aes(x = date, y = rate, col = imd_q5)) +
  scale_color_manual(values=c(imd_colors),name="IMD quintile") +
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.95),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()

plot0


#add black line to legend to represent number of events per week
key_colors <- append(imd_colors, "black")
key_colors = key_colors[1:length(key_colors)]


plot2 <- plot0 +
  scale_y_continuous(sec.axis = sec_axis(trans =~ . *150,
                              name = "Weekly number of events"),
                     expand = c(0,0),
                     #breaks = 4,
                     limits = c(0,5)) +
  geom_line(data = df_timeseries, aes(x = date, y = n_events/150),
            linewidth = 0.5, col = "black") +
  theme(axis.title.y.right = element_text(color = "black")
  ) +
  scale_linetype_manual(values = c("Number of events")) +
  scale_color_manual(values=c(key_colors), name = "", drop = F) +
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.95),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))



plot2

  ggsave(plot = plot2,width = 10, height=6,
         filename = "outputs/Figure2_descriptive.png")

#drop number of events from imd_q5 for analysis
df_timeseries$imd_q5 <- droplevels(df_timeseries$imd_q5)




####Define functions for regression models  ####

#function to fit poisson regression model for each IMD quintile
fit_glm <- function (df = df_timeseries, imd) {

#filter by IMD
 df <- df |>
       filter(imd_q5 == {imd})
#convert time into years
df$t <- df$t/52

#set t2 as time since july 2021
df$t2 <- df$t_since_jul21
#convert this to years too
df$t2 <- df$t2/52

#fit quasipossoin model
 mod <- glm(data = df,
            family = quasipoisson(link = "log"),
            formula = finishers ~ offset(log(total_pop)) +
            t +
            I(t^2) +
            four_week + covid +
            post_covid +
            t2
            +I(t2^2)
            )



  return(mod)
}


df_timeseries_total <- df_timeseries |>
  group_by(date, covid, post_covid, four_week) |>
  summarise(finishers = sum(finishers),
            total_pop = sum(total_pop),
            t = mean(t),
            t2 = mean(t_since_jul21))

#same as above but to fit model for total population
fit_glm_total <- function (df = df_timeseries_total) {

  df$t <- df$t/52

  df$t2 <- df$t2/52

  mod <- glm(data = df,
             family = quasipoisson(link = "log"),
             formula = finishers ~ offset(log(total_pop)) +
               t +
               I(t^2) +
               four_week + covid +
               post_covid +
               t2
             +I(t2^2)
  )



  return(mod)
}


#function to predict finisher numbers
pred_func <- function(df = df_timeserie,imd) {

#fit the same model as above
  df <- df |>
    filter(imd_q5 == {imd})

  df$t <- df$t/52

  df$t2 <- df$t_since_jul21

  df$t2 <- df$t2/52

  mod <- glm(data = df,
             family = quasipoisson(link = "log"),
             formula = finishers ~ offset(log(total_pop)) +
               t +
               I(t^2) +
               four_week + covid +
               post_covid +
               t2
             +I(t2^2)
  )

#use model to predict number of finishers based on df_timeseries data
  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd_q5 = {imd},
                   four_week = df$four_week,
                   covid_period = df$covid_period,
                   date = df$date)
#calculate participation rate for visualisation
  dt$rate =  dt$finishers *1000/df$total_pop
  return(dt)
}

#Function to extract model coefficients and covert to rate ratios (i.e. exponentiate them)
coef_table <- function(mod, imd) {



  mod_sum <- broom::tidy(mod, exp = T, conf.int = T)



  mod_sum <- data.table(
             imd_q5 = {imd},
             term = mod_sum$term,
             estimate = mod_sum$estimate,
             lower_bound = mod_sum$conf.low,
             upper_bound = mod_sum$conf.high)

  mod_sum <- mod_sum[mod_sum$term == "post_covid"|
                       mod_sum$term == "t2"|
                       mod_sum$term == "I(t2^2)"|
                       mod_sum$term == "t"|
                       mod_sum$term == "I(t^2)",]


  return(mod_sum)

}

####Fit models for each IMD quintile and create regression coefficients table####
#fit model for each IMD and extract relevant coefficients
reg_table <- bind_rows(coef_table(fit_glm(df_timeseries, "Most deprived 20%"), imd = "Most deprived 20%"),
          coef_table(fit_glm(df_timeseries, "2"), imd = "2"),
          coef_table(fit_glm(df_timeseries, "3"), imd = "3"),
          coef_table(fit_glm(df_timeseries, "4"), imd = "4"),
          coef_table(fit_glm(df_timeseries, "Least deprived 20%"), imd = "Least deprived 20%"),
          coef_table(fit_glm_total(df_timeseries_total), imd = "total population"))

#set all values to three significant figures
reg_table <- reg_table |>
  mutate(across(.cols = c(estimate,lower_bound,upper_bound),~round(.,2)))
#combine OR and 95% CI
reg_table$rate_ratio <- str_c(reg_table$estimate, " (", reg_table$lower_bound,"-",reg_table$upper_bound,")", sep = "" )
reg_table <- reg_table[,c("term", "imd_q5", "rate_ratio")]
#pivot wider for report
reg_table <- pivot_wider(data = reg_table , names_from = term, values_from = rate_ratio )
#output regression table
#write.csv(reg_table, "outputs/reg_table.csv")

####Apply prediction function to all quintiles and store as dataframe####
preds <- bind_rows(pred_func(df_timeseries, "Most deprived 20%"),
                    pred_func(df_timeseries, "2"),
                    pred_func(df_timeseries, "3"),
                    pred_func(df_timeseries, "4"),
                    pred_func(df_timeseries, "Least deprived 20%"))

####Model checks####
#check model fit - residual plot
mod1 <- fit_glm(df_timeseries,4)

res <- data.table(t = 0:n_weeks,
           residuals = mod1$residuals)

plot(x = res$t, y = res$residuals, ylim = c(-1,2))
#add lines to show covid period - large residuals when trial events begin to be run
abline(v = 342, col = "red", lwd = 2)
abline(v = 272, col = "red", lwd = 2)




####Generate counterfactual####
#function to fit model for each IMD based on pre-covid trends and predict
#finishers based on this
fit_glm_ex <- function (df = df_timeseries, imd) {

#filter by IMD
  df <- df |>
    filter(imd_q5 == {imd})

#convert t into years
df$t <- df$t/52


#fit regression model
  mod <- df |>
    filter(covid_period == "pre-lockdown") |>
    glm(family = quasipoisson(link = "log"),
        formula = finishers ~ offset(log(total_pop))
                  + t +I(t^2) + four_week)


#predict weekly finishes
  dt <- data.table(t = seq(1:(nrow(df))),
                   finishers = predict(mod, df, type = "response"),
                   imd_q5 = {imd},
                   four_week = df$four_week,
                   covid_period = df$covid_period,
                   date = df$date)

#convert finishes into rate for visualisation
  dt$rate =  dt$finishers *1000/df$total_pop

 # plot(mod)


  return(dt)
}

#Dataframe of counterfactual finisher predictions
preds_ex <- bind_rows(fit_glm_ex(df_timeseries, "Most deprived 20%"),
                   fit_glm_ex(df_timeseries, "2"),
                   fit_glm_ex(df_timeseries, "3"),
                   fit_glm_ex(df_timeseries, "4"),
                   fit_glm_ex(df_timeseries, "Least deprived 20%"))




####create smoothed predictions accounting for seasonality####
#use total population parameters for all as this is sufficient for visualisation
#fit model for total population
mod_total <- fit_glm_total(df_timeseries_total)

#define pred_smooth: a dataframe for smoothed predictions
pred_smooth <- df_timeseries
#extract rate ratios for each four-week period
four_week_coef <- tidy(mod_total, exp =T) |>
  filter(grepl("four_week", term)) |>
  #number in accordance to the 'four-week' variable in df_timeseries
  mutate(four_week = c(2:13)) |>
  select(four_week, estimate)
#add in a rate ratio of one for four_week = 1
four_week_coef[13,] <- list(1,1)
#make four_week a factor
four_week_coef$four_week <- as.factor(four_week_coef$four_week)
#join rate ratios to dataframe
pred_smooth <- left_join(pred_smooth, four_week_coef, by = "four_week")
#multiple rates by rate ratios to obtain smoothed predictions
pred_smooth$pred <- pred_smooth$rate * pred_smooth$estimate

####Plot counterfactuals and  observed data####
#ensure imd_q5 in preds_ex is a factor with the correct levels
preds_ex$imd_q5 <- factor(preds_ex$imd_q5, levels = imd_levels)

plot3 <-  plot0 +
  #add lines for counterfactual (without seasonality)
  geom_smooth(data = preds_ex,
                       aes(x = date, y = rate, col = imd_q5),
                       method = "glm",
                       #method.args = list(family = "poisson"),
                       formula = y ~ poly(x,2),
                       linetype = "dashed",
                       linewidth = 0.6,
                       se = F) +
  #add lines for observed data post pandemic period
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",],
            aes(x = date, y = rate, col = imd_q5),
            linewidth = 0.3) +
  #add in trend lines for observed data post pandemic period
  geom_smooth(data = pred_smooth[pred_smooth$covid_period == "post-lockdown",],
              aes(x = date, y = pred, col = imd_q5),
              se = F,
              method = "glm", formula = y ~ x + I(x^2)) +
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.95),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))+
  geom_vline(xintercept = 2021-01-07)

plot3
#save plot 3
ggsave(plot = plot3,width = 10, height=6,
      filename = "outputs/Figure3_preds.png")


####plot observed and unsmoothed counterfactuals for each IMD quintile####
#plot pre-pandemic observed participation
plot4 <- ggplot(df_timeseries[df_timeseries$covid_period == "pre-lockdown",],) +
  ylab(label = "Weekly finishers per 1000 residents")+
  xlab(label = "Date") +
  geom_line(linewidth = 0.3, aes(x = date, y = rate)) +
  theme_classic() +
  xlim(as.Date(c('1/1/2015', '1/1/2023'), format="%d/%m/%Y") ) +
  ylim(c(0, 5)) +
  #add lines for post-pandemic observed participation
  geom_line(data = df_timeseries[df_timeseries$covid_period == "post-lockdown",]
            ,
            aes(x = date, y = rate),
            linewidth = 0.3) +
  #add pre-pandemic modelled data
  geom_line(data = preds[preds$covid_period == "pre-lockdown",], aes(x = date, y = rate), linewidth = 0.8, col = "blue") +
  #plot for each IMD quintile seperately
  facet_wrap(~factor(imd_q5,levels = rev(imd_levels)), ncol = 1) +
  #add counterfactual participation for the pandemic and post-pandemic periods
  geom_line(data = preds_ex
  [preds_ex$covid_period == "post-lockdown"|
            preds_ex$covid_period == "lockdown",],
  aes(x = date, y = rate),
  col = "blue",
  linetype = "solid",
  linewidth = 1)

plot4
#save plot 4
ggsave(plot = plot4,width = 10, height=12,
       filename = "outputs/figure4_preds_by_imd.png")






####calculate number of participants lower than expected####
#subset post covid for predicted and observed datasets
preds_pc <- preds_ex[covid_period != "pre-lockdown"]
obs_pc <- df_timeseries[covid_period != "pre-lockdown"]
obs_pc$imd_q5 <- obs_pc$imd_q5

#merge the datasets
df_lost <- merge(preds_pc,
                 obs_pc,
                 by = c("date", "imd_q5"))


#keep desired columns
df_lost <- data.table(date = df_lost$date,
                      imd_q5 = df_lost$imd_q5,
                      predicted  = df_lost$finishers.x,
                      observed = df_lost$finishers.y,
                      #calculate 'lost finishes'
                      lost = df_lost$finishers.x - df_lost$finishers.y,
                      covid_period = df_lost$covid_period.x)

#sum lost finishers by imd - March 2020 - Feb 2023
total_lost <- df_lost |>
  group_by(imd_q5) |>
  summarise(predicted = round(sum(predicted),0),
            observed = round(sum(observed),0),
            lost = round(sum(lost),0)) |>
  mutate(total_lost = sum(lost)) |>
  mutate(perc_decrease = signif(lost *100/predicted,3))

#sum for period after reopening
reopen_lost <- df_lost |>
  filter(covid_period == "post-lockdown") |>
  group_by(imd_q5) |>
  summarise(predicted = round(sum(predicted),0),
            observed = round(sum(observed),0),
            lost = round(sum(lost),0)) |>
  mutate(total_reopen_lost = sum(lost)) |>
  mutate(perc_decrease = signif(lost *100/predicted,3))

#combine into table
lost_table <- left_join(reopen_lost, total_lost, by = "imd_q5")
#output table
write.csv(lost_table, "outputs/lost_table.csv")



###extrapolate models for total population to ID week###
####when the counterfactual and observed participation meet####
#create dataset with extrapolated dates
df_timeseries_extrap <- data.table(date = seq(from = as.Date("2015-01-03"),
                                                to = as.Date("2024-12-20"),
                                              by = "week"))

df_timeseries_extrap$t <- seq(1:nrow(df_timeseries_extrap))/52

df_timeseries_extrap$four_week <- rep(seq(1:13),
                                  each = 4,
                                  times = nrow(df_timeseries_extrap)/52)
df_timeseries_extrap$four_week <- as.factor(df_timeseries_extrap$four_week)

df_timeseries_extrap$covid <- ifelse(df_timeseries_extrap$date > "2020-03-21"
                                     & df_timeseries_extrap$date < as.Date("2021-07-17"),
                                     1,
                                     0)

df_timeseries_extrap$post_covid <- ifelse(df_timeseries_extrap$date > "2021-07-17",
                                     1,
                                     0)

df_timeseries_extrap$t2 <- difftime(df_timeseries_extrap$date,
                                        covid_period[2],
                                        units = c("weeks"))

df_timeseries_extrap$t2[df_timeseries_extrap$t2 < 1] <- 0
#convert to numeric
df_timeseries_extrap$t2 <- as.numeric(df_timeseries_extrap$t2)/52

df_timeseries_extrap$total_pop <- mean(df_timeseries_total$total_pop)
#fit model based on pre-covid trends (i.e. the counterfactual)
fit_glm_ex_total <-  function (df = df_timeseries_total) {

  #convert t into years
  df$t <- df$t/52


  #fit regression model
  mod <- df |>
    filter(date < "2020-03-21") |>
    glm(family = quasipoisson(link = "log"),
        formula = finishers ~ offset(log(total_pop))
        + t +I(t^2) + four_week)

  return(mod)
}

mod_total <- fit_glm_total(df_timeseries_total)
mod_total_ex <- fit_glm_ex_total(df_timeseries_total)

pred_extrap_obs <- data.table(date = df_timeseries_extrap$date,
                              finishers = predict(mod_total, df_timeseries_extrap, type = "response"),
                              post_covid = df_timeseries_extrap$post_covid)

pred_extrap_exp <- data.table(date = df_timeseries_extrap$date,
                              finishers =  predict(mod_total_ex, df_timeseries_extrap, type = "response"),
                              post_covid = df_timeseries_extrap$post_covid)
#Visualise extrapolated participation to check
plot(x=pred_extrap_obs$date, y = pred_extrap_obs$finishers)
plot(x=pred_extrap_exp$date, y = pred_extrap_exp$finishers)

#remove pre-reopening values
pred_extrap_obs <- pred_extrap_obs[pred_extrap_obs$post_covid == 1]

pred_extrap_exp <- pred_extrap_exp[pred_extrap_exp$post_covid == 1]
#extrace week that observed finishers overtakes counterfactual finishers
converge_week <- head(pred_extrap_obs[pred_extrap_obs$finishers > pred_extrap_exp$finishers],1)
converge_week









