

## function written by Quinn Webber to generate overlap between red squirrel territory polygons and points

get_intersection <- function(poly, spdf, n) {
  
  ## poly = list of polygons
  ## spdf = list of spdf 
  ## n = levels for the loop

  # Intersection between polygon and points ---------------------------------
  intersect_out <- c()
  for(i in 1:n){
  
  intersection <- st_intersection(x = poly[[i]], y = spdf[[i]])
  
  intersect_out[[i]] <- intersection 
  }

  intersect_out
    
}