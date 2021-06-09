
## calculate repeatability

### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load models
outIn <- readRDS("output/models/mcmc_instrength-outstrength.RDS")
areaOut <- readRDS("output/models/mcmc_area-outstrength.RDS")
areaIn <- readRDS("output/models/mcmc_area-intstrength.RDS")

## rep from out/in-strength model
rep_out_KL <- outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] +
     outIn$VCV[,"traitoutstrengthScale:gridKL.units"])
rep_out_SU <- outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] + 
     outIn$VCV[,"traitoutstrengthScale:gridSU.units"])

rep_in_KL <- outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     outIn$VCV[,"traitinstrengthScale:gridKL.units"])
rep_in_SU <- outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     outIn$VCV[,"traitinstrengthScale:gridSU.units"])

## rep from area/in-strength model
rep_area_KL <- areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"] + 
     areaIn$VCV[,"traitareaScale:gridKL.units"])
rep_area_SU <- areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"] +
     areaIn$VCV[,"traitareaScale:gridSU.units"]) 

rep_in_KL2 <- areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] + 
     areaIn$VCV[,"traitinstrengthScale:gridKL.units"])
rep_in_SU2 <- areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     areaIn$VCV[,"traitinstrengthScale:gridSU.units"])

## rep from area/out-strength model
rep_area_KL2 <- areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"] + 
     areaOut$VCV[,"traitareaScale:gridKL.units"])
rep_area_SU2 <- areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"] +
     areaOut$VCV[,"traitareaScale:gridSU.units"]) 

rep_out_KL2 <- areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] + 
     areaOut$VCV[,"traitoutstrengthScale:gridKL.units"])
rep_out_SU2 <- areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] +
     areaOut$VCV[,"traitoutstrengthScale:gridSU.units"])


repAll <- data.table(rep = c(mean(rep_out_KL), mean(rep_out_SU),
                             mean(rep_in_KL), mean(rep_in_SU), 
                             mean(rep_area_KL), mean(rep_area_SU),
                             mean(rep_in_KL2), mean(rep_in_SU2), 
                             mean(rep_area_KL2), mean(rep_area_SU2),
                             mean(rep_out_KL2), mean(rep_out_SU2)),
                     lwr = c(HPDinterval(rep_out_KL)[1], HPDinterval(rep_out_SU)[1],
                             HPDinterval(rep_in_KL)[1], HPDinterval(rep_in_SU)[1],
                             HPDinterval(rep_area_KL)[1], HPDinterval(rep_area_SU)[1],
                             HPDinterval(rep_in_KL2)[1], HPDinterval(rep_in_SU2)[1],
                             HPDinterval(rep_area_KL2)[1], HPDinterval(rep_area_SU2)[1],
                             HPDinterval(rep_out_KL2)[1], HPDinterval(rep_out_SU2)[1]),
                     upr = c(HPDinterval(rep_out_KL)[2], HPDinterval(rep_out_SU)[2],
                             HPDinterval(rep_in_KL)[2], HPDinterval(rep_in_SU)[2],
                             HPDinterval(rep_area_KL)[2], HPDinterval(rep_area_SU)[2],
                             HPDinterval(rep_in_KL2)[2], HPDinterval(rep_in_SU2)[2],
                             HPDinterval(rep_area_KL2)[2], HPDinterval(rep_area_SU2)[2],
                             HPDinterval(rep_out_KL2)[2], HPDinterval(rep_out_SU2)[2]),
                     Grid = c("KL", "SU", "KL", "SU", 
                              "KL", "SU", "KL", "SU", 
                              "KL", "SU", "KL", "SU"),
                     trait = c("Out-strength", "Out-strength", 
                               "In-strength", "In-strength",
                               "Territory size", "Territory size",
                               "In-strength", "In-strength", 
                               "Territory size", "Territory size",
                               "Out-strength", "Out-strength"),
                     Model = c("In-strength~Out-strength", "In-strength~Out-strength", 
                               "In-strength~Out-strength", "In-strength~Out-strength",
                               "In-strength~Territory size", "In-strength~Territory size", 
                               "In-strength~Territory size", "In-strength~Territory size",
                               "Out-strength~Territory size", "Out-strength~Territory size", 
                               "Out-strength~Territory size", "Out-strength~Territory size"))
                     

png("figures/Fig5.png", height = 2500, width = 4000, units = "px", res = 500)
ggplot(repAll) +
  geom_point(aes(rep, trait, 
             col = Grid, 
             shape = Model), 
             position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes(y = trait, 
                     xmin = lwr, 
                     xmax = upr,
                     color = Grid, 
                     shape = Model), 
                 position = position_dodge(width = 0.4), height = 0) +
  ylab("") + 
  xlab("Repeatability") +
  scale_color_manual(values = c("#f1a340", "#998ec3")) +
  theme(#legend.position = 'none',
       legend.title = element_text(size = 14, color = "black"),
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()
