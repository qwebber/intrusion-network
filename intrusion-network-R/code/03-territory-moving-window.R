


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

## load census
census <- fread("output/census_all.csv")
census <- census[!is.na(squirrel_id)]
## change name of squirrel_id in census to owner to merge with edge_list
setnames(census, "squirrel_id", "owner")
census$owner <- as.character(census$owner)

## filter to only include owner and sex variables
census <- census[,c("owner", "Sex")]

#remove duplicated rows
census <- census[!duplicated(census)]

## add intruder id
census$intruder <- census$owner

## add sex of owner and sex of intruder
edge_list <- merge(census[,c("owner", "Sex")], edge_list, by = "owner", all = T)
setnames(edge_list, "Sex", "owner_sex")
edge_list <- merge(census[,c("intruder", "Sex")], edge_list, by = "intruder", all = T)
setnames(edge_list, "Sex", "intruder_sex")

edge_list <- edge_list[!is.na(owner)][!is.na(intruder)]

## add min and max julian days for squirrel territories separately for owners and intruders
edge_list[, minDay := min(julian), by = c("owner", "grid", "year", "dyad")][, maxDay := max(julian), by = c("owner", "grid", "year", "dyad")]
edge_list[, minDayOwn := min(julian), by = c("owner", "grid", "year")][, maxDayOwn := max(julian), by = c("owner", "grid", "year")]
edge_list[, minDayInt := min(julian), by = c("intruder", "grid", "year")][, maxDayInt := max(julian), by = c("intruder", "grid", "year")]

## 1: presence of a squirrel within the lifetime of the territory (i.e. true intrusion)

edge_list[, during := minDayInt > minDayOwn & maxDayInt < maxDayOwn & edge == 1]
edge_list[, before := minDayInt < minDayOwn & edge == 1]
edge_list[, after := maxDayInt > maxDayOwn & edge == 1]

## true edge list that only includes intrusions that occur during the lifetime of a territory
#edge_list[after == "TRUE"]

## load census data
census_all <- readRDS("output/auxilliary-data/census-all.RDS")

census_all <- census_all[!is.na(squirrel_id)]

## determine if territory was owned by squirrel in previous year
census_all[, meanX := mean(locX), by = c("squirrel_id", "grid", "year")][, meanY := mean(locY), by = c("squirrel_id", "grid", "year")]

census_all[, row := seq_len(.N), by = c("squirrel_id", "grid", "year", "meanX", "meanY")]

census_all <- plyr::ddply(census_all, c('squirrel_id', 'year'))

setDT(census_all)

## determine if territory was held in the previous year
census_all <- census_all[row == 1][ , hold_terr_last_X := (meanX = shift(meanX)), by = "squirrel_id"][ , hold_terr_last_Y := (meanY = shift(meanY)), by = "squirrel_id"]
census_all$ownedX_last <- census_all$meanX - census_all$hold_terr_last_X
census_all$ownedY_last <- census_all$meanY - census_all$hold_terr_last_Y

## assign a territory as "owned" in consecutive years if all coordinates were with 1 unit in consecutive years
census_all[,ownedLastYear := ownedX_last < 1 & ownedX_last >-1 & ownedY_last <1 & ownedY_last >-1]

## determine if territory was held in the next year
census_all <- census_all[row == 1][ , hold_terr_next_X := (meanX = shift(meanX, type = "lead")), by = "squirrel_id"][ , hold_terr_next_Y := (meanY = shift(meanY, type = "lead")), by = "squirrel_id"]
census_all$ownedX_next <- census_all$meanX - census_all$hold_terr_next_X
census_all$ownedY_next <- census_all$meanY - census_all$hold_terr_next_Y

## assign a territory as "owned" in consecutive years if all coordinates were with 1 unit in consecutive years
census_all[,ownedNextYear := ownedX_next < 1 & ownedX_next >-1 & ownedY_next <1 & ownedY_next >-1]

census_all$own_gr_year <- as.factor(paste(census_all$squirrel_id, census_all$year, census_all$grid, sep = "_"))
edge_list$own_gr_year <- as.factor(paste(edge_list$owner, edge_list$year, edge_list$grid, sep = "_"))

## merge edge list with territory ownership to make new edge list file
edge_list2 <- merge(edge_list, 
                    census_all[,c("own_gr_year", "squirrel_id", "ownedLastYear", "ownedNextYear")], 
                    by = "own_gr_year", 
                    all = T)

## remove instanes where year == NA
edge_list2 <- edge_list2[!is.na(year)]

## remove intrusions that happened before territory was established AND when it was not owned last year
edge_list2[,beforeCut := (before == TRUE & ownedLastYear == FALSE)]

## remove intrusions that happened after territory was estalbished AND when it was not owned in the next year
edge_list2[,afterCut := (after == TRUE & ownedNextYear == FALSE)]

## convert NAs to "unknown"
edge_list2$beforeCut[is.na(edge_list2$beforeCut)] <- "unknown"
edge_list2$afterCut[is.na(edge_list2$afterCut)] <- "unknown"

edge_list2[, .N, by = "beforeCut"]


edge_list3 <- edge_list2[beforeCut != "TRUE"]  #& afterCut != "TRUE"]



302804 - 299711

920 + 2290

edge_list2[squirrel_id == 12613]

saveRDS(edge_list, "output/edge-list-true.RDS")

ggplot(census_all) +
  geom_histogram(aes(ownedY))

#hist(census_all$owned)

### Example figure
ggplot(edge_list[owner == 12613]) +
  geom_jitter(aes(julian, intruder, color = intruder), 
              height = 0.01, alpha = 0.25)  +
  geom_errorbar(aes(y=intruder, xmin=minDay, xmax=maxDay, color = intruder), 
                width=0.2, size=0.1, height = 0.2) 
  facet_wrap(~year)

