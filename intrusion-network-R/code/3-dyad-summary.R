

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load pedigree data

con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                    dbname ="krsp",
                    username = Sys.getenv("krsp_user"),
                    password = Sys.getenv("krsp_password")
)

ped <- tbl(con, "pedigree_clean") %>%
  collect()
setDT(ped)

## remove NAs from dam_id column
ped <- ped[!is.na(ped$dam_id)]

## add column for unique dyad ID of moms and offspring
ped$dyad <- as.factor(paste(ped$squirrel_id, ped$dam_id, sep = "_"))
ped$mom <- "yes"

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list <- rbindlist(edge_list)

## add column for unique dyad ID
edge_list$dyad <- as.factor(paste(edge_list$owner, edge_list$intruder, sep = "_"))

## merge to form file of just moms and their offspring
mom_offspring <- merge(edge_list, ped[,c("dyad", "mom")], by = "dyad")

## generate new edgelist file without moms and their offspring
edge_list <- edge_list %>% 
                filter(!dyad %in% mom_offspring$dyad)

## summary of observations that occurred on own territory vs. on a different territory
edge_list$territory <- as.factor(paste(edge_list$owner, 
                                       edge_list$locx, 
                                       edge_list$locy, 
                                       edge_list$year, sep = "_"))

edge_list[, c("year", "grid") := tstrsplit(year, "_", fixed=TRUE)]


df <- fread("output/spatial-locs.csv")
df$territory <- as.factor(paste(df$squirrel_id, 
                                df$locx, 
                                df$locy, 
                                df$year, sep = "_"))



merge(edge_list[year == "2016"], df[year == "2016"], by = "territory")


df[, .N, by = c("year", "grid")]

