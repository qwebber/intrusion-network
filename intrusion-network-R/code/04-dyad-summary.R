

### Packages ----
libs <- c('data.table',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load spatial locs
df <- fread("output/spatial-locs.csv")
df$territory <- as.factor(paste(df$squirrel_id, 
                                df$locx, 
                                df$locy, 
                                df$julian, 
                                df$year, sep = "_"))


## merge file with edges to file with behaviours
df_edges <- merge(edge_list[,c("edge", "territory")], 
                  df, by = "territory", allow.cartesian = TRUE)

## get rid of duplicate rows
df_edges <- df_edges %>% 
             distinct(.keep_all = TRUE)

## rename behaviours 
df_edges$behaviour[df_edges$behaviour == 0] <- "other"
df_edges$behaviour[df_edges$behaviour == 1] <- "feeding"
df_edges$behaviour[df_edges$behaviour == 2] <- "vocalizations"
df_edges$behaviour[df_edges$behaviour == 3] <- "travelling"
df_edges$behaviour[df_edges$behaviour == 4] <- "resting"
df_edges$behaviour[df_edges$behaviour == 5] <- "in nest"
df_edges$behaviour[df_edges$behaviour == 6] <- "off territory"
df_edges$behaviour[df_edges$behaviour == 7] <- "interaction"
df_edges$behaviour[df_edges$behaviour == 8] <- "caching"
df_edges$behaviour[df_edges$behaviour == 9] <- "dead"
df_edges$behaviour[df_edges$behaviour == 10] <- "grooming"
df_edges$behaviour[df_edges$behaviour == 11] <- "playing"
df_edges$behaviour[df_edges$behaviour == 12] <- "foraging"
df_edges$behaviour[df_edges$behaviour == 13] <- "out of sight"
df_edges$behaviour[df_edges$behaviour == 14] <- "nest building"
df_edges$behaviour[df_edges$behaviour == 15] <- "unknown"
df_edges$behaviour[df_edges$behaviour == 16] <- "scent marking"
df_edges$behaviour[df_edges$behaviour == 17] <- "moving kids"
df_edges$behaviour[df_edges$behaviour == 18] <- "trapped off territory"
df_edges$behaviour[df_edges$behaviour == 19] <- "vigilant"
df_edges$behaviour[df_edges$behaviour == 20] <- "digging for truffles"
df_edges$behaviour[df_edges$behaviour == 21] <- "foot stomping"
df_edges$behaviour[df_edges$behaviour == 22] <- "mating chase"
df_edges$behaviour[df_edges$behaviour == "x"] <- "unknown"
df_edges$behaviour[df_edges$behaviour == ""] <- "unknown"


## summary of behaviours on own territory (0) and other squirrels teritory (1)
sum <- df_edges[, .N, by = c("edge", "behaviour")]
ddply(sum, c('N'))


ggplot()