
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
rep_out <- outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] +
     outIn$VCV[,"traitoutstrengthScale.units"])

rep_in <- outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     outIn$VCV[,"traitinstrengthScale.units"])

## rep from area/in-strength model
rep_area <- areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"] + 
     areaIn$VCV[,"traitareaScale.units"])

rep_in2 <- areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] + 
     areaIn$VCV[,"traitinstrengthScale.units"])

## rep from area/out-strength model
rep_area2 <- areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"] + 
     areaOut$VCV[,"traitareaScale.units"])

rep_out2 <- areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] + 
     areaOut$VCV[,"traitoutstrengthScale.units"])


repAll <- data.table(rep = c(mean(rep_out), mean(rep_out2),
                             mean(rep_in), mean(rep_in2), 
                             mean(rep_area), mean(rep_area2)),
                     lwr = c(HPDinterval(rep_out)[1], HPDinterval(rep_out2)[1],
                             HPDinterval(rep_in)[1], HPDinterval(rep_in2)[1],
                             HPDinterval(rep_area)[1], HPDinterval(rep_area2)[1]),
                     upr = c(HPDinterval(rep_out)[2], HPDinterval(rep_out2)[2],
                             HPDinterval(rep_in)[2], HPDinterval(rep_in2)[2],
                             HPDinterval(rep_area)[2], HPDinterval(rep_area2)[2]),
                     trait = c("Out-strength", "Out-strength", 
                               "In-strength", "In-strength",
                               "Territory size", "Territory size"),
                     Model = c("In-strength~Out-strength", "Out-strength~Territory size", 
                               "In-strength~Out-strength", "In-strength~Territory size",
                               "In-strength~Territory size", "Out-strength~Territory size")) 
                     

png("figures/Fig5.png", height = 2500, width = 4000, units = "px", res = 500)
ggplot(repAll) +
  geom_point(aes(rep, trait, 
             color = Model), 
             position = position_dodge(width = 0.4),
             size = 4) +
  geom_errorbarh(aes(y = trait, 
                     xmin = lwr, 
                     xmax = upr,
                     color = Model), 
                 position = position_dodge(width = 0.4), 
                 size = 1,
                 height = 0) +
  xlim(0,1) +
  ylab("") + 
  xlab("Repeatability") +
  scale_color_manual(values = c("#f1a340", "#998ec3", "#5aae61")) +
  theme(#legend.position = 'none',
       legend.title = element_text(size = 14, color = "black"),
        legend.key = element_blank(),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
dev.off()
