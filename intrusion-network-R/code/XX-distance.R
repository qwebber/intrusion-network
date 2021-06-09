

census_all <- readRDS("output/auxilliary-data/census-all.RDS")
census_all <- census_all[grid == "KL" & grid == "SU" |
             year >= 1996]

n<-length(census_all$squirrel_id)

census_all$social_survival<-NULL
census_all$social_repro<-NULL

for (j in 1:n) {
  temp<-subset(census_all, census_all$grid==census_all$grid[j]&census_all$year==census_all$year[j]&census_all$squirrel_id!=census_all$squirrel_id[j]) # consider only those observations from the same grid and year
  temp$distance<-sqrt((30*(temp$locx-census_all$locx[j]))^2+(30*(temp$locy-census_all$locy[j]))^2)
  n2<-length(temp$squirrel_id)
  for (i in 1:n2) {        
    temp$fraction[i]<-length(subset(d_distance, d_distance > temp$distance[i]))/length(d_distance)
  }
  temp$surv_frac<-temp$survived*temp$fraction
  temp$surv_frac2<-temp$survived2*temp$fraction
  temp$repro_frac<-temp$all_litters_fit*temp$fraction
  
  census_all$social_survival[j]<-sum(temp$surv_frac, na.rm=T)
  census_all$social_survival2[j]<-sum(temp$surv_frac2, na.rm=T)
  census_all$social_repro[j]<-sum(temp$repro_frac, na.rm=T)
}
