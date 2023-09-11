agg_parkrun_stat = function(df,y){

  fig_df = eval(parse(text = paste("aggregate(",y," ~ imd_q5 + month_year, df, mean)")))

  fig_df$plot_date = as.Date(paste(fig_df$month_year,"15",sep="-"))

  fig_general = eval(parse(text = paste("aggregate(",y," ~ month_year, df, mean)")))

  fig_general$plot_date = as.Date(paste(fig_general$month_year,"15",sep="-"))

  fig_general$imd_q5 = "Overall"

  fig_general = fig_general[,names(fig_df)]

  fig_df = rbind(fig_df, fig_general)

  return(fig_df)

}


make_parkrun_tbl = function(x,r=2){
  m = round(mean(x),r)
  se = round(sqrt(var(x)/length(x)),r)
  res = paste(m," (",se,")",sep="")
  return(res)
}
