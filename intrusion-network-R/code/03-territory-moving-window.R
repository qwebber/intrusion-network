


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

edge_list$group <- 1:length(edge_list$owner)

## remove the intruder column
edge_list_owner <- edge_list[, c("owner", "owner_sex", 
                                 "locx", "locy", "julian","edge", 
                                 "year", "dyad", "grid",
                                 "gr_year", "group")]
edge_list_owner$direction <- "owner"
setnames(edge_list_owner, c("owner_sex", "owner"), c("sex", "squirrel_id"))
## remove the owner column
edge_list_intruder <- edge_list[, c("intruder", "intruder_sex", 
                                 "locx", "locy","julian","edge", 
                                 "year", "dyad", "grid",
                                 "gr_year", "group")]
edge_list_intruder$direction <- "intruder"
setnames(edge_list_intruder, c("intruder_sex", "intruder"), c("sex", "squirrel_id"))

## add min and max julian days for squirrel territories separately for owners and intruders
edge_list[, minDay := min(julian), by = c("owner", "grid", "year", "dyad")][, maxDay := max(julian), by = c("owner", "grid", "year", "dyad")]
edge_list[, minDayOwn := min(julian), by = c("owner", "grid", "year")][, maxDayOwn := max(julian), by = c("owner", "grid", "year")]
edge_list[, minDayInt := min(julian), by = c("intruder", "grid", "year")][, maxDayInt := max(julian), by = c("intruder", "grid", "year")]

## 1: presence of a squirrel within the lifetime of the territory (i.e. true intrusion)

edge_list[, during := minDayInt > minDayOwn & maxDayInt < maxDayOwn & edge == 1]
edge_list[, before := minDayInt < minDayOwn & edge == 1]
edge_list[, after := maxDayInt > maxDayOwn & edge == 1]

## true edge list that only includes intrusions that occur during the lifetime of a territory
edge_list_true <- edge_list[during == "TRUE"]

saveRDS(edge_list_true, "output/edge_list_true.RDS")



### Example figure
ggplot(edge_list[owner == 12613]) +
  geom_jitter(aes(julian, intruder, color = intruder), 
              height = 0.01, alpha = 0.25)  +
  geom_errorbar(aes(y=intruder, xmin=minDay, xmax=maxDay, color = intruder), 
                width=0.2, size=0.1, height = 0.2) 
  facet_wrap(~year)

