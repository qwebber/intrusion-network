

###############################################
################### FIGURE 4 ################### 
###############################################

### Packages ----
libs <- c('data.table', 'dplyr', 'MCMCglmm',
          'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

## load models
outIn <- readRDS("output/models/mcmc_instrength-outstrength.RDS")
areaOut <- readRDS("output/models/mcmc_area-outstrength.RDS")
areaIn <- readRDS("output/models/mcmc_area-intstrength.RDS")


## out-strength - in-strength
df_outIn <- data.table(Trait = attr(colMeans(outIn$Sol), "names"),
                               Value = colMeans(outIn$Sol)) 
df_outIn2 <- df_outIn[59:length(df_outIn$Trait),]
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
cor_in_out <- outIn$VCV[,"traitinstrengthScale:traitoutstrengthScale.squirrel_id"]/
  (sqrt(outIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"])*
     sqrt(outIn$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]))

mean(cor_in_out)
HPDinterval(cor_in_out)


## out-strength - territory size
df_outArea <- data.table(Trait = attr(colMeans(areaOut$Sol), "names"),
                       Value = colMeans(areaOut$Sol)) 
df_outArea2 <- df_outArea[59:length(df_outArea$Trait),]
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
cor_area_out <- areaOut$VCV[,"traitareaScale:traitoutstrengthScale.squirrel_id"]/
  (sqrt(areaOut$VCV[,"traitareaScale:traitareaScale.squirrel_id"])*
     sqrt(areaOut$VCV[,"traitoutstrengthScale:traitoutstrengthScale.squirrel_id"]))

mean(cor_area_out)
HPDinterval(cor_area_out)


## in-strength - territory size
df_inArea <- data.table(Trait = attr(colMeans(areaIn$Sol), "names"),
                         Value = colMeans(areaIn$Sol)) 
df_inArea2 <- df_inArea[59:length(df_inArea$Trait),]
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

## CORRELATIONS between Intercept in-strength and Intercept in-strength:
cor_area_in <- areaIn$VCV[,"traitareaScale:traitinstrengthScale.squirrel_id"]/
  (sqrt(areaIn$VCV[,"traitareaScale:traitareaScale.squirrel_id"])*
     sqrt(areaIn$VCV[,"traitinstrengthScale:traitinstrengthScale.squirrel_id"]))

mean(cor_area_in)
HPDinterval(cor_area_in)


png("figures/Fig4.png", width = 4000, height = 4000, res = 600, units = "px")
fig4a <- ggplot(outInDF, aes(interceptOut, interceptIn)) +
  geom_point(size = 2, alpha = 0.65) +
  #geom_smooth(method = "lm") +
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
  #geom_smooth(method = "lm") +
  ylab("Area") +
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
  geom_smooth(method = "lm") +
  ylab("Area") +
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

ggplot(all, aes(outstrength, instrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "loess") 
ggplot(all, aes(area_ha, instrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "loess") 
ggplot(all, aes(area_ha, outstrength)) +
  geom_point(aes(color = as.factor(year))) +
  theme(legend.position = 'none') + 
  geom_smooth(method = "lm") 
grid.arrange(aa,bb,cc, nrow = 1)  
