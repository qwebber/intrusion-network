
### Packages ----
libs <- c('data.table', 'igraph', 'ggraph',
          'ggplot2', 'krsp', 'sp', 'adehabitatHR',
          'sf')
lapply(libs, require, character.only = TRUE)

## load data
df <- fread("output/spatial-locs.csv")

## load matrices
out_mats <- readRDS("output/matrices/matrix_list.RDS")


## filter for squirrels with at least 15 observations
## first assign dummy column to count number of observations per ID in each year and grid
df[, row := seq_len(.N), by = c("grid", "year")]
df[, N := uniqueN(row), by = c("squirrel_id","grid", "year")]

## drop all squirrels with <30 observations
df <- df[N > 30]

## order dataframe by grid and year
df <- setDT(ddply(df, c('year', 'grid')))
df <- df[grid == "KL" & year == "2016"]

#### generate polygon ####
## prj
prj <- '+init=epsg:26911'
params = c(grid = 400, extent = 3)

## load GetHRBy function
source("functions/GetHRBy.R")

ud <- df[, GetHRBy(squirrel_id, locx, locy, 
                    in.percent = 75, params = params,
                    type = "kernel")]

## assign prj
proj4string(ud) <- CRS(prj)

## mean coordinates 
coordsMeans <- data.frame(squirrel_id = unique(df$squirrel_id),
                          locx = df[, median(locx), by = "squirrel_id"]$V1,
                          locy = df[, median(locy), by = "squirrel_id"]$V1)

## convert to intrusion network
out_mats$`2016_KL`[lower.tri(out_mats$`2016_KL`)] <- 0
KL_2016out <- graph.adjacency(out_mats$`2016_KL`, weighted = T)

## make sure same individuals in both files
mets <- data.table(outstrength = graph.strength(KL_2016out, mode = c("out")),
                 squirrel_id = names(degree(KL_2016out)))
                        
coordsMeans2 <- coordsMeans %>% 
                  filter(squirrel_id %in% mets$squirrel_id)
         
setnames(coordsMeans, c("locy", "locx"), c("y", "x"))

KL2016 = create_layout(KL_2016out, layout = coordsMeans2) # algorithm = 'kk')


ggplot(ud, aes(x = long, y = lat, fill = id, group = group)) + 
  geom_polygon(alpha = 0.4) +
  theme(legend.position = 'none')

bb <- ggplot(KL2016) +
  geom_node_point(aes(color = factor(squirrel_id)),
                  size = (log(graph.strength(KL_2016out, mode = c("out")))*4), 
                  alpha = 0.5) +
  geom_edge_link(alpha = 0.5) +
  theme(legend.position = 'none')
grid.arrange(aa,bb, nrow = 1)


ggplot(KL2016) +
       #layout = 'manual', 
       #node.positions = coordsMeans[,2:3]) + 
   + 
  geom_node_point(size = (graph.strength(KL_2016out)*0.05),
    alpha = 0.75) +
  


  # scale_edge_width(range=c(0.1,2)) +
  ggtitle("KL 2016") +
  ylab('') +
  xlab('') +
  #coord_fixed() +
  scale_color_viridis_d() +
  theme(#legend.position = 'none',
    legend.key = element_blank(),
    axis.text=element_text(size=12, color = "black"),
    axis.title=element_text(size=12),
    strip.text = element_text(size=12,face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))

