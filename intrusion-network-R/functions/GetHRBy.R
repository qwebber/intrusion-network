


### HR by block ----
GetHRBy <- function(by.col, x.col, y.col, in.percent, type, params = NULL){
  ids <- data.frame(by.col)
  coordinates(ids) <- cbind(x.col, y.col)
  #crs(ids) <- in.crs
  if(type == 'kernel'){
    kernel <- do.call(kernelUD, c(ids, params))
    vert <- getverticeshr(kernel, percent = in.percent, unout = "ha")  
  } else if(type == 'mcp'){
    vert <- do.call(mcp, c(xy = ids, percent = in.percent, params))
  } else {
    stop('please provide either kernel or mcp for type')
  }
}