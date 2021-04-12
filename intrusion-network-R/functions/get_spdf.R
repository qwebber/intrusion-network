

get_spdf <- function(df, n){
  
  ## df = input dataframe
  ## n = levels for the loop
    
  out <- c()

  for(i in levels(n)){ 
  
  df2 <- df[gr_year == i] ## this is very specific to my dataset and not easily transferable - so, must have a colulmn called "gr_yeaR" to run
  
  spdf <- SpatialPointsDataFrame(coordinates(cbind(df2$locx, df2$locy)), ## locs must be "locx" and "locy" to run
                                 data = df2,
                                 proj4string = CRS(prj))
  
  out[[i]] <- st_as_sf(spdf)
  
  }
  out
}
