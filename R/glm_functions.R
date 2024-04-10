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
