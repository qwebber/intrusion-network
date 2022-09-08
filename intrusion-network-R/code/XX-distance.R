


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf', 'spatsoc', 'dils',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)

## load flastall

## load database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL")) %>% 
  dplyr::select(squirrel_id, gr, sex, byear=byear, dam_id, bcert=bcert)

flastall <- setDT(collect(flastall))
flastall$squirrel_id <- as.factor(flastall$squirrel_id)
flastall$owner <- flastall$squirrel_id 
flastall$intruder <- flastall$squirrel_id 



## load spatial data
df <- readRDS("output/spatial-locs.RDS")
df$squirrel_id <- as.character(df$squirrel_id)
df$gr_year <- as.character(df$gr_year)

df2 <- df[, median(locx), by = c("squirrel_id", "gr_year")]
df2$locy <- df[, median(locy), by = c("squirrel_id", "gr_year")]$V1 
setnames(df2, "V1", "locx")

df_nn <- edge_dist(df2, id = "squirrel_id", coords = c("locx", "locy"), 
                    timegroup = NULL, threshold = 10000, returnDist = T, 
                    splitBy = "gr_year")

df_nn$dyad <- as.factor(paste(df_nn$ID1, df_nn$ID2, df_nn$gr_year, sep = "_"))

## load edge list data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$grid, edge_list$year, sep = "_"))
yr <- fread("output/unique-grid-years.csv")

yr <- data.table(gr_year = unique(edge_list$gr_year))

## number of unique grid-years
n <- length(unique(yr$gr_year))
gr_year <- unique(yr$gr_year)

## number of obs per owner
edge_list[, Nowner := .N, by = c("owner", "grid", "year")]

## number of obs per intruder
edge_list[, Nintruder := .N, by = c("intruder", "grid", "year")]

edge_list <- edge_list[edge != 0]

## generate metrics
out2 <- c()
for(i in 1:n){ 
  
  k <- gr_year[i]
  
  df1 <- edge_list[gr_year == k][, .N, by = c("owner", "intruder", "Nintruder", "gr_year")]
  
    
  ## generate territoriality index (intrusions/intrusions + total locs)
  df2 <- data.table(owner = df1$owner,
                    intruder = df1$intruder, 
                    Nintruder = df1$Nintruder)
    
  adj <- AdjacencyFromEdgelist(df2)
    
  diag(adj$adjacency) <- 0
  
  adj2 <- as.matrix(adj$adjacency)
  colnames(adj2) <- adj$nodelist
  rownames(adj2) <- adj$nodelist
  
  out1 <- data.table(setNames(melt(adj2), c('owner', 'intruder', 'Nintruder')))
  out1$gr_year <- k  
  
  ## generate territoriality index (intrusions/intrusions + total locs)
  out2[[i]] <- out1
  
  
}

out2 <- rbindlist(out2, fill = T)
out2[, c("grid", "year") := tstrsplit(gr_year, "_", fixed=TRUE)]
out2$dyad <- as.factor(paste(out2$owner, out2$intruder, out2$gr_year, sep = "_"))

aa <- merge(out2[, c("gr_year") := NULL], df_nn, by = "dyad")
aa[, c("dyad", "ID1", "ID2") := NULL]
aa$dyad <- as.factor(paste(aa$owner, aa$intruder, sep = "_"))
aa$owner <- as.factor(aa$owner)
aa$intruder <- as.factor(aa$intruder)

aa <- aa[gr_year != "KL_2006"]


## merge intrusion data and flastall based on owner id
aa2 <- merge(aa,flastall, by = "owner")
aa2[, c("intruder.y") := NULL]
setnames(aa2, c("intruder.x", "sex", "byear", "dam_id", "bcert"),
               c("intruder", "sex_owner", "byear_owner", "dam_id_own", "bcert_own"))

## merge intrusion data and flastall based on ownder id
aa3 <- merge(aa2[,c("squirrel_id", "gr") := NULL], 
             flastall[, c("owner", "squirrel_id", "gr") := NULL], 
             by = "intruder")

setnames(aa3, c("sex", "byear", "dam_id", "bcert"),
         c("sex_intruder", "byear_intruder", "dam_id_intruder", "bcert_intruder"))

aa3$age_owner <- as.numeric(aa3$year) - as.numeric(aa3$byear_owner)
aa3$age_intruder <- as.numeric(aa3$year) - as.numeric(aa3$byear_intruder)

aa3$delta_age <- abs(aa3$age_owner - aa3$age_intruder)

aa3 <- aa3[!is.na(sex_intruder)]
aa3 <- aa3[!is.na(sex_owner)]


ggplot(aa3, aes(distance, Nintruder)) +
  geom_point() +
  geom_smooth() 

aa3$distance <- round(aa3$distance)

a2 <- lmer(Nintruder ~ poly(distance) + (1|dyad), data = aa3)

plot(Nintruder ~ distance, data = aa3)

predict(a2, newdata = data.frame(distance = seq(0, 819),
                                 dyad = "NA"))
  
aa3$pred <- data.table(predict(a2))

ggplot(aa3) +
  geom_point(aes(distance, pred))

        


mdl1 <- lm(Nintruder ~ age_intruder, data = aa3[grid == "KL"])
mdl2 <- lm(Nintruder ~ age_intruder + I(age_intruder^2), data = aa3[grid == "KL"])
mdl3 <- lm(Nintruder ~ age_intruder + I(age_intruder^2) + I(age_intruder^3), data = aa3[grid == "KL"])
mdl4 <- lm(Nintruder ~ I(age_intruder^2), data = aa3[grid == "KL"])

prd <- data.frame(age_intruder = seq(0, 7, by = 0.5))

result <- prd
result$mdl1 <- predict(mdl1, newdata = prd)
result$mdl2 <- predict(mdl2, newdata = prd)
result$mdl3 <- predict(mdl3, newdata = prd)
result$mdl4 <- predict(mdl4, newdata = prd)

library(reshape2)
library(ggplot2)

result <-  melt(result, id.vars = "age_intruder", variable.name = "model",
                value.name = "fitted")


ggplot(data = aa3[grid == "KL"], 
       aes(x = delta_age, y = scale(Nintruder))) + #result, aes(x = age_intruder, y = fitted)) +
  geom_jitter()  +
  geom_smooth(method = "loess") +
  theme_bw() + 
  facet_wrap(~sex_owner*sex_intruder, scale = "free") 


mod2 <- lmer(scale(Nintruder) ~ 
               poly(delta_age, degree = 5) + 
               sex_owner + 
               (1|dyad), 
             data = aa3[grid == "SU" & sex_intruder == "M"])
summary(mod2)

visreg(mod2)
