



### Packages ----
libs <- c('data.table',
          'ggplot2')
lapply(libs, require, character.only = TRUE)

### load data 
all <- readRDS("output/final-df.RDS")
all$squirrel_id <- as.factor(all$squirrel_id)
all$year <- as.factor(all$year)

unique(all$gr_year)

all$area_ha <- all$area_m2/10000

all <- all[!is.na(all$sex)]
all <- all[!is.na(all$grid)]

## Territory size
median(all[gr_year != "KL_2006"]$area_ha)
sd(all[gr_year != "KL_2006"]$area_ha)
median(all[gr_year == "KL_2006"]$area_ha)
sd(all[gr_year == "KL_2006"]$area_ha)


## Out-strength
median(all[gr_year != "KL_2006"]$outstrength)
sd(all[gr_year != "KL_2006"]$outstrength)
median(all[gr_year == "KL_2006"]$outstrength)
sd(all[gr_year == "KL_2006"]$outstrength)

## In-strength
median(all[gr_year != "KL_2006"]$instrength)
sd(all[gr_year != "KL_2006"]$instrength)
median(all[gr_year == "KL_2006"]$instrength)
sd(all[gr_year == "KL_2006"]$instrength)


png("figures/FigS8.png", width = 6000, height = 3000, units = "px", res = 600)
FigS8a <- ggplot() +
  geom_density(data = all[gr_year == "KL_2006"], aes(area_ha), 
               fill = "red", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year == "KL_2006"], 
             aes(xintercept = median(area_ha)), 
             lty = 2) +
  geom_density(data = all[gr_year != "KL_2006"], aes(area_ha), 
               fill = "blue", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year != "KL_2006"], 
             aes(xintercept = median(area_ha)), 
             lty = 2) +
  ylab("Density distribution") +
  xlab("Territory size (ha)") +
  ggtitle("A)") + 
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1)) 

FigS8b <- ggplot() +
  geom_density(data = all[gr_year == "KL_2006"], aes(outstrength), 
               fill = "red", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year == "KL_2006"], 
             aes(xintercept = median(outstrength)), 
             lty = 2) +
  geom_density(data = all[gr_year != "KL_2006"], aes(outstrength), 
               fill = "blue", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year != "KL_2006"], 
             aes(xintercept = median(outstrength)), 
             lty = 2) +
  ylab("Density distribution") +
  xlab("Out-intrusion-strength") +
  ggtitle("B)") +
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1)) 

FigS8c <- ggplot() +
  geom_density(data = all[gr_year == "KL_2006"], aes(instrength), 
               fill = "red", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year == "KL_2006"], 
             aes(xintercept = median(instrength)), 
             lty = 2) +
  geom_density(data = all[gr_year != "KL_2006"], aes(instrength), 
               fill = "blue", 
               alpha = 0.5) +
  geom_vline(data = all[gr_year != "KL_2006"], 
             aes(xintercept = median(instrength)), 
             lty = 2) +
  ylab("Density distribution") +
  xlab("In-intrusion-strength") +
  ggtitle("C)") +
  xlim(0,1.1) +
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1)) 

grid.arrange(FigS8a, FigS8b, FigS8c,nrow = 1)
dev.off()
