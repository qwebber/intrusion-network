

###############################################
################### FIGURE 4 ################### 
###############################################

### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load models
mcmc_in_out <- readRDS("output/models/mcmc_instrength-outstrengthScale.RDS")
mcmc_in_terr <- readRDS("output/models/mcmc_area-instrength.RDS")
mcmc_out_terr <- readRDS("output/models/mcmc_area-outstrength.RDS")


## out-strength - in-strength
df_outIn <- data.table(Trait = attr(colMeans(mcmc_in_out$Sol), "names"),
                               Value = colMeans(mcmc_in_out$Sol)) 
df_outIn2 <- df_outIn[15:length(df_outIn$Trait),]
df_outIn2[, c('Trait' ,'xx' ,'squirrel_id') := tstrsplit(Trait, '.', fixed = TRUE)][,c("xx") := NULL]

df_outIn2$Trait[df_outIn2$Trait == "traitinstrengthScale"] <- 'inStrength'  
df_outIn2$Trait[df_outIn2$Trait == "traitoutstrengthScale"] <- 'outStrength'  
df_outIn2$Trait[df_outIn2$Trait == "traitinstrengthScale:spr_density"] <- 'plastInStr'  
df_outIn2$Trait[df_outIn2$Trait == "traitoutstrengthScale:spr_density"] <- 'plastOutStr'  


outInDF <- data.table(interceptIn = df_outIn2[Trait == "inStrength"]$Value,
                    interceptOut = df_outIn2[Trait == "outStrength"]$Value,
                    slopeIn = df_outIn2[Trait == "plastInStr"]$Value, 
                    slopeOut =  df_outIn2[Trait == "plastOutStr"]$Value, 
                    squirrel_id = df_outIn2[Trait == "inStrength"]$squirrel_id)
 
## CORRELATIONS between Intercept in-strength and Intercept out-strength:
cor_in_out <- mcmc_in_out$VCV[,"traitinstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (sqrt(mcmc_in_out$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"])*
     sqrt(mcmc_in_out$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]))

mean(cor_in_out)
HPDinterval(cor_in_out)


## out-strength - territory size
df_outArea <- data.table(Trait = attr(colMeans(mcmc_out_terr$Sol), "names"),
                       Value = colMeans(mcmc_out_terr$Sol)) 
df_outArea2 <- df_outArea[15:length(df_outArea$Trait),]
df_outArea2[, c('Trait' ,'xx' ,'squirrel_id') := tstrsplit(Trait, '.', fixed = TRUE)][,c("xx") := NULL]

df_outArea2$Trait[df_outArea2$Trait == "traitareaScale"] <- 'area'  
df_outArea2$Trait[df_outArea2$Trait == "traitoutstrengthScale"] <- 'outStrength'  
df_outArea2$Trait[df_outArea2$Trait == "traitareaScale:spr_density"] <- 'plastArea'  
df_outArea2$Trait[df_outArea2$Trait == "traitoutstrengthScale:spr_density"] <- 'plastOutStr'  


outAreaDF <- data.table(interceptArea = df_outArea2[Trait == "area"]$Value,
                      interceptOut = df_outArea2[Trait == "outStrength"]$Value,
                      slopeArea = df_outArea2[Trait == "plastArea"]$Value, 
                      slopeOut =  df_outArea2[Trait == "plastOutStr"]$Value, 
                      squirrel_id = df_outArea2[Trait == "outStrength"]$squirrel_id)

## CORRELATIONS between Intercept in-strength and Intercept out-strength:
cor_area_out <- mcmc_out_terr$VCV[,"traitareaScale:traitoutstrengthScale.squirrel_id"]/
  (sqrt(mcmc_out_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"])*
     sqrt(mcmc_out_terr$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]))

mean(cor_area_out)
HPDinterval(cor_area_out)


## in-strength - territory size
df_inArea <- data.table(Trait = attr(colMeans(mcmc_in_terr$Sol), "names"),
                         Value = colMeans(mcmc_in_terr$Sol)) 
df_inArea2 <- df_inArea[15:length(df_inArea$Trait),]
df_inArea2[, c('Trait' ,'xx' ,'squirrel_id') := tstrsplit(Trait, '.', fixed = TRUE)][,c("xx") := NULL]

df_inArea2$Trait[df_inArea2$Trait == "traitareaScale"] <- 'area'  
df_inArea2$Trait[df_inArea2$Trait == "traitinstrengthScale"] <- 'inStrength'  
df_inArea2$Trait[df_inArea2$Trait == "traitareaScale:spr_density"] <- 'plastArea'  
df_inArea2$Trait[df_inArea2$Trait == "traitinstrengthScale:spr_density"] <- 'plastinStr'  


inAreaDF <- data.table(interceptArea = df_inArea2[Trait == "area"]$Value,
                        interceptin = df_inArea2[Trait == "inStrength"]$Value,
                        slopeArea = df_inArea2[Trait == "plastArea"]$Value, 
                        slopein =  df_inArea2[Trait == "plastinStr"]$Value, 
                        squirrel_id = df_inArea2[Trait == "inStrength"]$squirrel_id)

## CORRELATIONS between Intercept area and Intercept in-strength:
cor_area_in <- mcmc_in_terr$VCV[,"traitareaScale:traitinstrengthScale.squirrel_id"]/
  (sqrt(mcmc_in_terr$VCV[,"traitareaScale:traitareaScale.squirrel_id"])*
     sqrt(mcmc_in_terr$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]))

mean(cor_area_in)
HPDinterval(cor_area_in)




png("figures/FigX.png", width = 6000, height = 3000, res = 600, units = "px")
fig4a <- ggplot(outInDF, aes(interceptOut, interceptIn)) +
  geom_point(size = 2, alpha = 0.65) +
  #geom_smooth(method = "lm", color = "darkblue", se = F) +
  ylab("In-intrusion-strength") +
  xlab("Out-intrusion-strength") +
  ggtitle("A)") +
  theme(legend.position = c(0.1, 0.9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        plot.title = element_text(size = 20),
        axis.title=element_text(size=20),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

fig4b <- ggplot(outAreaDF, aes(interceptOut, interceptArea)) +
  geom_jitter(size = 2, alpha = 0.65) +
  #geom_smooth(method = "lm", color = "darkblue", se = F) +
  ylab("Territory size (ha)") +
  xlab("Out-intrusion-strength") +
  ggtitle("B)") +
  theme(legend.position = c(0.1, 0.9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        plot.title = element_text(size = 20),
        axis.title=element_text(size=20),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

fig4c <- ggplot(inAreaDF, aes(interceptin, interceptArea)) +
  geom_jitter(size = 2, alpha = 0.65) +
  #geom_smooth(method = "lm", color = "darkblue", se = F) +
  ylab("Territory size (ha)") +
  xlab("In-intrusion-strength") +
  ggtitle("C)") +
  theme(legend.position = c(0.1, 0.9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        plot.title = element_text(size = 20),
        axis.title=element_text(size=20),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

grid.arrange(fig4a, fig4b, fig4c, nrow = 1)

dev.off()



aa <- ggplot(all, aes(outstrength, instrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "loess") 
bb <- ggplot(all, aes(area_ha, instrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "loess") 
cc <- ggplot(all, aes(area_ha, outstrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "lm") 
grid.arrange(aa,bb,cc, nrow = 1)  


ggplot(all, aes(sex, outstrength)) +
  geom_boxplot(notch = T,
               outlier.color = NA) +
  geom_jitter(alpha = 0.5)
ggplot(all, aes(sex, instrength)) +
  geom_boxplot(notch = T, 
               outlier.color= NA) +
  geom_jitter(alpha = 0.5)
ggplot(all, aes(sex, area_ha)) +
  geom_boxplot(notch = T, 
               outlier.color= NA) +
  geom_jitter(alpha = 0.5)


