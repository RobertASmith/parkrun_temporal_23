# custom function to determine the distance to the nearest parkrun event
# for any given month
get_min_dist <- function(available_event_cols = T,
                        distance_matrix  = distM,
                        month_year="2010-01"){

  # subset matrix to get distance only for available events.
  Mat = distance_matrix[ , available_event_cols]

  # loop through LSOA finding minimum distances.
  min_dist = apply(X = Mat,
                   MARGIN = 1,
                   FUN = min)

  # create data-frame with LSOA code, month and distance to nearest event.
  min_dist_df = data.frame(lsoa = rownames(Mat),
                           month_year=month_year,
                           access=min_dist)

  # return the data-frame
  return(min_dist_df)

}
