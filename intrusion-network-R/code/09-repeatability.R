
## calculate repeatability

### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm')
lapply(libs, require, character.only = TRUE)

## load models
mcmc_in_out <- readRDS("output/models/mcmc_instrength-outstrengthScale.RDS")
mcmc_in_terr <- readRDS("output/models/mcmc_area-instrength.RDS")
mcmc_out_terr <- readRDS("output/models/mcmc_area-outstrength.RDS")

## rep from out/in-strength model
rep_out <- mcmc_in_out$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (mcmc_in_out$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] +
     mcmc_in_out$VCV[,"traitoutstrengthScale.units"])

rep_in <- mcmc_in_out$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (mcmc_in_out$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     mcmc_in_out$VCV[,"traitinstrengthScale.units"])


## rep from out-strength and territory size model
rep_area <- mcmc_out_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (mcmc_out_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"] +
     mcmc_out_terr$VCV[,"traitareaScale.units"])

rep_out2 <- mcmc_out_terr$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (mcmc_out_terr$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"] +
     mcmc_out_terr$VCV[,"traitoutstrengthScale.units"])

## rep from in-strength and territory size model
rep_area2 <- mcmc_in_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"]/
  (mcmc_in_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"] +
     mcmc_in_terr$VCV[,"traitareaScale.units"])

rep_in2 <- mcmc_in_terr$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]/
  (mcmc_in_terr$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"] +
     mcmc_in_terr$VCV[,"traitinstrengthScale.units"])



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
                     Model = c("Outstrength-Instrength", "Outstrength-Territory Size", 
                               "Outstrength-Instrength", "Instrength-Territory Size", 
                               "Outstrength-Territory Size", "Instrength-Territory Size"))

repAll                     



png("figures/FigX.png", height = 2500, width = 4000, units = "px", res = 500)
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
