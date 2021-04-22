

## function written by Quinn Webber to generate red squirrel territory polygons

get_polygon <- function(df, n, yr, in.percent, params) {
  
  ## load GetHRBy function
  source("functions/GetHRBy.R")
  
  ## df = input dataframe
  ## n = levels for the loop
  ## id = identity of animals
  ## in.percent = kernel %
  ## params = parameters for kernels (see GetHRBy function)
  
  out_polygon <- c()
  
  for(i in 1:n){ 
    
    k = as.character(yr[i])  
    
    df3 <- df[gr_year == k]
    
    ## generate ranges by ID
    ud <- df3[, GetHRBy(squirrel_id, ## id must be squirrel_id
                        locx, locy, ## coords must be locx and locy
                        in.percent = in.percent, 
                        params = params,
                        type = "kernel")]
    
    ## assign prj
    proj4string(ud) <- CRS(prj)
    
    # Get polygon
    polygon <- st_as_sf(ud)
    
    # convert to sf object
    colnames(polygon) <- c("id_polygons", "area" ,"geometry") # change colnames
    
    out_polygon[[i]] <- polygon
    
  }  
  
  out_polygon
}