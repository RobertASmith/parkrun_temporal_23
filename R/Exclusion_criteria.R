####exclusion criteria####
#1. discontinued parkruns#
#reorder
df_event <- readRDS(file = "data/clean/dt_events_ts.rds")

discontinued <- function(df ) {

  # df_startdate <- df |>
  # arrange(eventdate) |>
  # distinct(eventname, .keep_all = T) |>
  # select(c(eventname, "startdate" = eventdate))



df_enddate <- df |>
  arrange(desc(eventdate)) |>
  distinct(eventname, .keep_all = T) |>
  rename(enddate = eventdate) |>
  mutate(discont = case_when(enddate == "2023-02-25" ~ 0,
                             enddate > "2020-02-01" ~ 1,
                             enddate < "2020-02-01" ~ 0))

df_enddate <- df_enddate[, c("eventname", "discont")]


df2 <- left_join(df, df_enddate, by = "eventname") |>
  filter(discont == 0 )

df2 <- df2[,-"discont"]


return(df2)

}



#2. weeks where events cancelled
#collapse df_event into n events by week
cancelled <- function(df) {
df_cancelled <- df |>
  distinct(eventname, date) |>
  group_by(date) |>
  summarise(n_event = n()) |>
  #add 4-week roling mean
  mutate(r_mean = rollmean(n_event, k = 4, fill = n_event)) |>
  #exclude weeks with <80% of rolling average number of events
  mutate(cancelled = ifelse(n_event < 0.8*(r_mean),1,0)) |>
  filter(cancelled == 1)

#df2 <- left_join(df, df_cancelled, by = "date") |>
#  filter(cancelled == 0)

df_cancelled <- df_cancelled[,"date"]

return(df_cancelled)
}

#check
df_event2 <- cancelled(df_event)
