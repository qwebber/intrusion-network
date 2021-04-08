

### Packages ----
libs <- c('data.table',
          'ggplot2', 'gridExtra',
          'lme4',
          'visreg')
lapply(libs, require, character.only = TRUE)

## load data
all <- fread ("output/final-df.csv")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

## model 1: outstrength
mod1 <- lmer(log(outstrength + 1) ~ #age + I(age^2) + 
                  grid + 
                  spr_density + 
                  I(spr_density^2) +
                  (1|year) + 
                  (1|squirrel_id), 
                  data=all)
summary(mod1)

## model 2: instrength
mod2 <- lmer(log(instrength + 1) ~ #age + I(age^2) + 
               grid + 
               spr_density + 
               I(spr_density^2) +
               (1|year) + 
               (1|squirrel_id), 
             data=all)
summary(mod2)

vis_mod1 <- visreg(mod1, "spr_density", xlab="Density", ylab="Out-strength") 
plot(vis_mod1)
vis_mod2 <- visreg(mod2, "spr_density", xlab="Density", ylab="In-strength") 


ggplot(all) +
  geom_point(aes(instrength, outstrength))

aa <- ggplot(filter(vis_mod1$fit), aes(spr_density, visregFit))+
          geom_line(colour = 'black', 
                    size=1)+
          geom_jitter(data = filter(vis_mod1$res),
                             aes(spr_density, visregRes), 
                             alpha = 0.8, 
                             color = "#d8b365") +
          xlab('Density') +
          ylab('Out-strength') + 
          theme(legend.position = c(0.75,0.8),
                legend.title = element_blank(),
                legend.key = element_blank(),
                legend.text = element_text(size = 10),
                axis.title = element_text(size = 14, color = 'black'),
                axis.text = element_text(size = 12, color = 'black'),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                panel.border = element_rect(colour = "black", fill=NA, size = 1))
bb <- ggplot(filter(vis_mod2$fit), aes(spr_density, visregFit))+
  geom_line(colour = 'black', 
            size=1)+
  geom_jitter(data = filter(vis_mod2$res),
              aes(spr_density, visregRes), 
              alpha = 0.8, 
              color = "#5ab4ac") +
  xlab('Density') +
  ylab('In-strength') + 
  theme(legend.position = c(0.75,0.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size = 1))
grid.arrange(aa,bb)
