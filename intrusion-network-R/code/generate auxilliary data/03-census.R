
libs <- c('data.table', 'sf', 'dplyr', 'sp',
          'lubridate','ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## KRSP connect
con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                    dbname ="krsp",
                    username = Sys.getenv("krsp_user"),
                    password = Sys.getenv("krsp_password")
)

#Importing midden census data
census_1 <- tbl(con, "dbamidden") %>% 
  collect() %>%
  dplyr::select(reflo, squirrel_id, locX, locY, grid, date, Sex) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  mutate(locX = loc_to_numeric(locX))

#Importing squirrel census data
census_2 <- tbl(con, "census") %>%
  # be careful with case, database uses locX in census, but locX in dbaMidden
  # sql doesn't care since it's not case sensitive, but R does!
  dplyr::select(reflo, squirrel_id, locx, locy, gr, census_date, sq_fate, sex) %>%
  filter(sq_fate != 7) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locx = loc_to_numeric(locx))

census_2 <- dplyr::select(census_2, -sq_fate) %>% 
  dplyr::rename(locX = locx,
                locY = locy,
                grid = gr,
                date = census_date,
                Sex = sex)

census_all<-bind_rows(census_1, census_2)%>% 
  mutate(grid = factor(grid),
         year = year(ymd(date)),
         month = month(ymd(date)),
         julian = yday(date),
         locY=as.numeric(locY),
         Sex=factor(Sex))

census_all$gr_year <- as.factor(paste(census_all$year, census_all$grid, sep = "_"))

## subset to only include census dates and remove NAs
## exclude years prior to 2000 and KL + SU grids
census_all <- setDT(census_all)[julian == 135 | julian == 136][grid == "KL" | grid == "SU"]

census_all <- census_all[!is.na(locX)][!is.na(locY)]

saveRDS(census_all, "output/auxilliary-data/census-all.RDS")