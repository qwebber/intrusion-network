
## function written by Quinn Webber to generate buffer around red squirrel midden locs


get_buffer <- function(df, n, buff){ 
  
  ## df = input dataframe
  ## n = levels for the loop
  
  out_buf <- c()
  
  for(i in 1:n) { 
    
    spdf <- df[[i]]
    
    out_buf[[i]] = st_buffer(spdf, buff)
    
  }
  out_buf
}