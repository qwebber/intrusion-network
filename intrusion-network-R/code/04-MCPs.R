


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'igraph', 'spatsoc',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)



con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

## pull behaviour database
behaviour <- tbl(con, "behaviour") %>% 
  filter(grid %in% c ("KL")) %>% 
  collect() %>% 
  mutate(locx = loc_to_numeric(locx),
         locy = as.numeric(locy)) 

## pull census database
census <- tbl(con, "census") %>% 
  filter(gr %in% c ("KL"),
         census_date == "2016-05-15") %>% 
  collect() %>% 
  dplyr::select(squirrel_id)

## add julian date, year, drop NAs
behaviour<-behaviour %>% 
  mutate(date=ymd(date),
         julian = yday(date),
         year = year(date),
         mode=as.factor(mode),
         squirrel_id = as.factor(squirrel_id)) %>% 
  filter(!is.na(squirrel_id),
         !is.na(locx),
         !is.na(locy)) %>% 
  droplevels()

## number of behaviours observed per yer 
behaviour %>% 
  group_by(year) %>% 
  summarize(n = n()) %>% 
  arrange (-n)

## filter to 2016 as an example year
behaviour_2016 <- behaviour %>% 
  filter (year == 2016,
          locx > -15,
          locx < 20,
          locy < 25,
          #behaviour == 2, ## vocalizations 
          detail == 1, ## animal material 
          squirrel_id %in% census$squirrel_id 
          #locx> 0,
          #locx < 12,
          #locy > 5,
          #locy < 15
  ) %>%
  droplevels()


splst <- behaviour_2016 %>% 
  group_by(squirrel_id) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>%
  filter(n > 15) %>% 
  dplyr::select(squirrel_id) %>% 
  extract2(1) %>% 
  as.character()
# Squirrels with more than 30 observations in 2016


behaviour_2016<-behaviour_2016 %>% 
  filter(squirrel_id %in% splst) %>% 
  droplevels()

## check to make sure there are no outliers
ggplot(behaviour_2016) +
  geom_point(aes(locx, locy, color = squirrel_id))

#prj <- '+init=epsg:26911'
#spdf <- SpatialPointsDataFrame(coordinates(cbind(behaviour_2016$locx, behaviour_2016$locy)),
#                              data = behaviour_2016, proj4string = CRS(prj))

#kd <- kernelUD(spdf[, 17], grid = 500, extent = 15) # squirrel_id is in column 17

source("functions/GetHRBy.R")

#behaviour_2016$squirrel_id2 <- as.factor(paste(behaviour_2016$squirrel_id, behaviour_2016$year, sep = "_"))

ud <- setDT(behaviour_2016)[, GetHRBy(squirrel_id, locx, locy, 50 ,"kernel")]

#image(kd)
#plot(getverticeshr(kd, percent = 75), add = TRUE)

#ud <- getverticeshr(kd, percent = 50)

#class(ud)
#plot(ud)
df <- fortify(ud)

territory_plot<-ggplot() +
  geom_polygon(data = ud, aes(x = long, y = lat, fill = id, group = group), alpha = 0.4) +
  geom_point(data = behaviour_2016, aes(locx, locy, color = squirrel_id), alpha = 0.5) +
  coord_equal() +
  #theme_void()+
  xlab("X")+
  #xlim(1,13) +
  #ylim (4.5,14) +
  ylab("Y")+
  ggtitle("2016 Territories") +  
  theme(legend.position = "none",
        axis.text=element_text(size=10),
        axis.title=element_text(size=12))

print(territory_plot)


saveRDS(vert.dt, "output/4-home-range-area.RDS")