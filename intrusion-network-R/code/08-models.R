

### Packages ----
libs <- c('data.table','dplyr')
lapply(libs, require, character.only = TRUE)

## load data
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

density <- readRDS("output/auxilliary-data/spring-density.RDS")
density <- setDT(density)[year > 1995] 
density <- density[grid == "KL" | grid == "SU"]

### Reaction Norm for KDE ----
p.var_str <- var(all$outstrength, na.rm = TRUE)

prior <- list(G=list(G1=list(V=diag(2)*(p.var_str/2), nu=1,
                                 alpha.V=diag(2)*p.var_str/2)),
                  R=list(V=diag(1)*(p.var_str/2), nu=1))

mcmc1 <- MCMCglmm(scale(outstrength) ~ 
                      grid +
                      sex + 
                      age + 
                      spr_density,
                    random =~ us(1 + spr_density):squirrel_id,
                    rcov = ~units,
                    family = "gaussian",
                    prior = prior,
                    #nitt=420000,
                    #burnin=20000,
                    #thin=100,
                    verbose = TRUE,
                    data = all,
                    pr=TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)

saveRDS(mcmc1, "output/models/mcmc_strength.RDS")

#####################
## TERRITORY SIZE ##
#####################

mcmc2 <- MCMCglmm(scale(area_ha) ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc2, "output/models/mcmc_territory.RDS")

