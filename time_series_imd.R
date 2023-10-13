
library(tidyverse)

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


#aggregate by month and by imd quintilex
df_timeseries = agg_parkrun_stat(lsoa_df_monthly,y="finishers")

#visualise trend over time
ggplot(fig2_df,aes(x=plot_date,
                            y=finishers,
                            col=imd_q5)) +
  ylab(label = "Monthly finishers per 1000 residents")+
 geom_point() +
 #geom_line(alpha=0.7,linewidth=0.3) +
#geom_smooth(alpha=1,se=F) +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  # ylab("Mean distance to the nearest parkrun event") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  theme_classic()+
  theme(legend.justification = c(1, 1),
        legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))

df_timeseries <- df_timeseries |>
  filter(imd_q5 != "Overall")
df_timeseries$total_pop <- sum(df_timeseries$finishers)
n_months <- nrow(df_timeseries)/5
df_timeseries$t <- rep(1:n_months,each=5 )

#fit simple segmented model with imd as a covariate
mod0 <- glm(data = df_timeseries,
            family = poisson(link = "log"),
            offset = log(total_pop),
            #subset = dt_timeseries$covid_period == "pre-lockdown",
            formula = finishers ~ t +imd_q5)
summary(mod0)

predicted_runs0 <- predict(object = mod0,
                           newdata = data.frame(t = as.numeric(df_timeseries$t),
                           imd_q5 = df_timeseries$imd_q5))

ggplot(data = df_timeseries,
       aes(x = t,
           y = finishers,
           col = imd_q5)) +
         geom_line()


lines(x = df_timeseries$t,
      y = predicted_runs0, col = "darkgreen")


