

### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'sf',
          'ggplot2', 'krsp')
lapply(libs, require, character.only = TRUE)


df <- fread("output/spatial-locs-15.csv")
df$squirrel_id <- as.character(df$squirrel_id)

yr <- fread("output/unique-grid-years.csv")

## prj
prj <- '+init=epsg:26911'

## load home made packages
source("functions/get_spdf.R")
source("functions/get_polygon.R")
source("functions/GetHRBy.R")

## load SPDF file
## save SPDF
spdf <- readRDS("output/edge_list_data/spdf.RDS")

## subset to KL 2015 and 2016
spdf2 <- list(spdf[[22]])

yr2 <- data.table(gr_year = as.factor(c("KL_2016")))

## parameters for kernel
params = c(grid = 700, extent = 4)

a1 <- df[gr_year == "KL_2018"]

a2 <- a1[N > 50]
a2 <- plyr::ddply(a2, c('squirrel_id'))
a2 <- setDT(a2)[, rowAll := seq_len(.N), by = c("squirrel_id")]

a3 <- a2[rowAll < 51]

a3[, sample(nrow(.SD), 10), by = "squirrel_id"]
a4 <- a3[,.SD[sample(.N, min(10,.N))],by = "squirrel_id"]


out <- c()
for(i in 1:45){ 

  samp <- 51 - i
  
  df_sub <- a3[,.SD[sample(.N, min(samp,.N))],by = "squirrel_id"]
  
  hrs <- df_sub[, GetHRBy(squirrel_id, ## id must be squirrel_id
                        locx, locy, ## coords must be locx and locy
                        in.percent = 50, 
                        params = params,
                        type = "kernel")]
  
  hrsDT <- data.table(id = hrs$id, 
                   area = hrs$area,
                   iter = 51 - i)
  
  out[[i]] <- hrsDT

}

saveRDS(out,"output/territories/KL2018-50-pts.RDS")

out <- readRDS("output/territories/KL2018-50-pts.RDS")

out2 <- rbindlist(out)

out2$iter <- as.numeric(out2$iter)

out2$area <- out2$area/10000

out2$area50 <- rep(out2[iter == 50]$area, 45)

out2$propArea <- out2$area/out2$area50

out3 <- out2[, mean(propArea), by = "iter"]

png("figures/Fig-territory-sensitivity.png", 
    height = 3000, 
    width = 3000,
    units = "px", 
    res = 600)
ggplot(out2, aes(iter, area)) +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), 
              se = T,
              color = "black") +
  geom_hline(yintercept = mean(out2[iter == 50]$area), lty = 2) +
  geom_vline(xintercept = 15, lty = 2) + 
  #geom_vline(xintercept = 20, lty = 2) + 
  ylab("Average territory size (ha)") +
  xlab("Number of locs") +
  ggtitle("n = 23 squirrels from KL 2018") +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12,face = "bold"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 
dev.off()



# Generate territorial polygons ---------------------------------
out_polygon <- get_polygon(df = df[gr_year == "KL_2016"], 
                           n = yr2$gr_year,
                           in.percent = 50,
                           params = params)

range(df[gr_year == "KL_2015" | gr_year == "KL_2016"][, .N, by = "squirrel_id"]$N)
