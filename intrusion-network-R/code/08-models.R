

### Packages ----
libs <- c('data.table','dplyr', 'MCMCglmm')
lapply(libs, require, character.only = TRUE)

## load data
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

## scale variables within year:
all[, outstrengthScale := scale(outstrength), by = c("year", "grid")]
all[, instrengthScale := scale(instrength), by = c("year", "grid")]
all[, areaScale := scale(area_ha), by = c("year", "grid")]

########################
#### SUMMARY STATS ####
#######################  

## number of unique individuals
length(unique(all$squirrel_id))

## number of squirrel years
length(unique(all$id_yr_gr))

## number of years per ID
mean(all[, .N, by = "squirrel_id"]$N)
sd(all[, .N, by = "squirrel_id"]$N)
range(all[, .N, by = "squirrel_id"]$N)

## average territory size mast years
all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, median(area_ha)]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(area_ha)]

## average territory size non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(area_ha)]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(area_ha)]


## average territory size mast years
all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, median(outstrength)]

all[year == 1998 |
      year == 2005 | 
      year == 2010 | 
      year == 2014 | 
      year == 2019][, sd(outstrength)]

## average intrusion strength non-mast years
all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, median(outstrength)]

all[year != 1998 |
      year != 2005 | 
      year != 2010 | 
      year != 2014 | 
      year != 2019][, sd(outstrength)]

############################ 
#### INTRUSION STRENGTH ####
############################ 
p.var_str <- var(all$outstrength, na.rm = TRUE)

prior_out <- list(G=list(G1=list(V=diag(2)*(p.var_str/2), nu=4,
                                 alpha.V=diag(2)*p.var_str/2)),
                  R=list(V=diag(1)*(p.var_str/2), nu=4))

mcmc1 <- MCMCglmm(outstrengthScale ~ 
                      grid +
                      sex + 
                      age + 
                      spr_density + 
                      year,
                    random =~ us(1 + spr_density):squirrel_id,
                    rcov = ~units,
                    family = "gaussian",
                    prior = prior_out,
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
p.var_area <- var(all$area_ha, na.rm = TRUE)

prior_area <- list(G=list(G1=list(V=diag(2)*(p.var_area/2), nu=4,
                                 alpha.V=diag(2)*p.var_area/2)),
                  R=list(V=diag(1)*(p.var_area/2), nu=4))

mcmc2 <- MCMCglmm(areaScale ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                    year,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_area,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc2, "output/models/mcmc_territory.RDS")


#####################
## IN-STRENGTH ##
#####################
p.var_in <- var(all$instrength, na.rm = TRUE)

prior_in <- list(G=list(G1=list(V=diag(2)*(p.var_in/2), nu=4,
                                  alpha.V=diag(2)*p.var_in/2)),
                   R=list(V=diag(1)*(p.var_in/2), nu=4))

mcmc3 <- MCMCglmm(instrengthScale ~ 
                    grid +
                    sex + 
                    age + 
                    spr_density + 
                    year,
                  random =~ us(1 + spr_density):squirrel_id,
                  rcov = ~units,
                  family = "gaussian",
                  prior = prior_in,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

saveRDS(mcmc3, "output/models/mcmc_instrength.RDS")


## Bivariate model
prior2 <- list(R = list(V=diag(4)*(p.var_str/2), nu=4),
               G = list(G1 = list(V = diag(4)*(p.var_str/2), nu=4,
                                      alpha.V = diag(4)*p.var_str/2)))

mcmc4 <- MCMCglmm(cbind(scale(instrength), 
                        scale(outstrength)) ~ 
                    trait-1 + 
                    trait:grid +
                    trait:sex + 
                    trait:age + 
                    trait:spr_density +
                    trait:year,
                  random =~ us(trait + spr_density:trait):squirrel_id,
                  rcov =~ idh(trait:grid):units,
                  family = c("gaussian","gaussian"),
                  prior = prior2,
                  #nitt=420000,
                  #burnin=20000,
                  #thin=100,
                  verbose = TRUE,
                  data = all,
                  pr=TRUE,
                  saveX = TRUE,
                  saveZ = TRUE)

summary(mcmc4)


##################  CORRELATIONS ##################
## CORRELATIONS between Intercept in-strength and Intercept out-strength:
cor_str_terr <- mcmc4$VCV[,"traitinstrength:traitoutstrength.squirrel_id"]/
  (sqrt(mcmc4$VCV[,"traitinstrength:traitinstrength.squirrel_id"])*
     sqrt(mcmc4$VCV[,"traitoutstrength:traitoutstrength.squirrel_id"]))

median(cor_str_terr)
HPDinterval(cor_str_terr)

ggplot(all) +
  geom_point(aes(instrength, outstrength, color = as.factor(year))) +
  theme(legend.position = 'none') 
  facet_wrap(~year)


cor.test(all$instrength, all$outstrength)
