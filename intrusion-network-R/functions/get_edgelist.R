
## function written by Quinn Webber to generate edge lists for red squirrel territory intrusions 

get_edgelist <- function(df, n){
  
  ## df = list of point-polygon intersection data
  ## n = levels for the loop
  
  edge_out <- c()
  for(i in 1:length(n)){ 
    ## generate edge list with territory owners and intruders
    edge_list <- data.table(owner = df[[i]]$id_polygons, 
                            intruder = df[[i]]$squirrel_id, ## id must be squirrel_id
                            locx = df[[i]]$locx, ## must be locx
                            locy = df[[i]]$locy, ## must be locy
                            julian = df[[i]]$julian,## ## must be julian
                            data = df[[i]]$data , ## trap or behavioural
                            behaviour = df[[i]]$behaviour, ## type of behaviour
                            mode =  df[[i]]$mode, ## type of data collection
                            detail = df[[i]]$detail) ## detatil about behaviour 
    ## assign TRUE or FALSE value to whether a squirrel is observed on 
    ## it's own territory (TRUE) or another territory (FALSE)
    edge_list[, edge:= (owner==intruder)]
    
    edge_list$edge <- as.character(edge_list$edge)
    
    ## re-assign TRUE and FALSE values to 0s and 1s
    edge_list$edge[edge_list$edge == "TRUE"] <- 0
    edge_list$edge[edge_list$edge == "FALSE"] <- 1
    
    ## add year to list 
    edge_list$year <- yr$gr_year[i]
    
    ## keep all points in file
    edge_out[[i]] <- edge_list
    
  }
  
  edge_out
  
}