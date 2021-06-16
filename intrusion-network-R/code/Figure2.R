
### Packages ----
libs <- c('data.table', 'igraph', 'ggraph',
          'ggplot2', 'krsp', 'sp', 'adehabitatHR',
          'sf', 'ggnetwork', 'dils',
          'patchwork')
lapply(libs, require, character.only = TRUE)


## load database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL")) %>% 
  dplyr::select(squirrel_id, gr, sex, byear=byear, dam_id, bcert=bcert)

flastall <- setDT(collect(flastall))

## load data
df <- readRDS("output/spatial-locs.RDS")

## load data
edge_list <- readRDS("output/edge-list-true.RDS")
edge_list[, Nintruder := .N, by = c("intruder", "grid", "year")]

## load territory polygons
polys <- readRDS("output/edge-list-inputs/polygons-all.RDS")

gr_year <- data.table(id = names(polys))


df1 <- edge_list[gr_year == "2015_KL"][, .N, by = c("owner", "intruder", "Nintruder")]

df2 <- data.table(owner = df1$owner,
                  intruder = df1$intruder, 
                  TI = df1$N/(df1$N + df1$Nintruder))

adj <- AdjacencyFromEdgelist(df2)

diag(adj$adjacency) <- 0

rownames(adj$adjacency) <- adj$nodelist
colnames(adj$adjacency) <- adj$nodelist


g <- graph_from_adjacency_matrix(adj$adjacency, weighted = TRUE, mode = c("directed")) 

plot(g)

##
df10 <- df[grid == "KL" & year == 2015]
coordsMeans <- data.frame(squirrel_id = as.factor(unique(df10$squirrel_id)),
                          locx = df10[, median(locx)/30, by = "squirrel_id"]$V1,
                          locy = df10[, median(locy)/30, by = "squirrel_id"]$V1)
setnames(coordsMeans, c("locy", "locx"), c("y", "x"))

gEdge <- get.data.frame(g)  # get the edge information using the get.data.frame function

head(gEdge)
gEdge$from.x <- coordsMeans$x[match(gEdge$from, coordsMeans$squirrel_id)]  #  match the from locations from the node data.frame we previously connected
gEdge$from.y <- coordsMeans$y[match(gEdge$from, coordsMeans$squirrel_id)]
gEdge$to.x <- coordsMeans$x[match(gEdge$to, coordsMeans$squirrel_id)]  #  match the to locations from the node data.frame we previously connected
gEdge$to.y <- coordsMeans$y[match(gEdge$to, coordsMeans$squirrel_id)]

polys2015 <- sf::st_transform(polys$KL_2015)
polys2015$gr_year <- rep("2015", length(polys2015$id_polygons))


### Plot points 
aa <- ggplot(df[gr_year == "KL_2015"]) +
  geom_jitter(aes(locx/30, locy/30, color = factor(squirrel_id)), 
             alpha = 0.25) + 
  ylab("") + xlab("") +
  ggtitle("A)") +
  geom_rect(aes(xmin = 2, xmax = 12, 
                ymin = 0, ymax = 8), color = "black", fill = "transparent") +
  scale_y_continuous(name = "", 
                     breaks = c(-4, 0,
                                4, 8,
                                12, 16, 20,
                                24), 
                     limits=c(-2,25))  +
  scale_x_continuous(name = "", 
                     breaks = c(-12, -8, 
                                -4, 0,
                                4, 8,
                                12, 16, 20), 
                     limits=c(-12,20)) + 
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

bb <- ggplot( df[gr_year == "KL_2015"]) + 
  geom_jitter(aes(locx/30, locy/30, color = factor(squirrel_id)), 
              alpha = 0.25) + 
  ylab("") + xlab("") +
  ggtitle("B)") +
  scale_y_continuous(name = "", 
                     breaks = c(2, 4, 6, 8, 10,
                                12, 14, 16), 
                     limits=c(0, 8))  +
  scale_x_continuous(name = "", 
                     breaks = c(2, 4, 6, 8, 10,
                                12, 14, 16), 
                     limits=c(2, 12)) +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

cc <- ggplot(data = polys2015) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  #coord_sf(datum = st_crs(32648)) 
  geom_rect(aes(xmin = 2*30, xmax = 12*30, 
                ymin = 0*30, ymax = 8*30), color = "black", fill = "transparent") + 
  ggtitle('C)') +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text= element_blank(),#$element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

dd <- ggplot(data = polys2015) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  ylim(0*30, 8*30) +
  xlim(2*30, 12*30) +
  ggtitle('D)') +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text= element_blank(),#$element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ee <- ggplot() + 
  geom_point(data = coordsMeans,
             aes(x, y, color = squirrel_id), 
             size = 5, 
             alpha = 0.5) +
  geom_segment(data=gEdge,
               aes(x = from.x, 
                   xend = to.x, 
                   y = from.y, 
                   yend = to.y),
               colour="black") +
  geom_rect(aes(xmin = 2, xmax = 12, 
                ymin = 0, ymax = 8), color = "black", fill = "transparent") +
  scale_y_continuous(name = "", 
                     breaks = c(-4, 0,
                                4, 8,
                                12, 16, 20,
                                24), 
                     limits=c(-2,25))  +
  scale_x_continuous(name = "", 
                     breaks = c(-12, -8, 
                                -4, 0,
                                 4, 8,
                                 12, 16, 20), 
                     limits=c(-12,20)) + 
  ggtitle('E)') +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ff <- ggplot() + 
  geom_point(data = coordsMeans,
             aes(x, y, color = squirrel_id), 
             size = 5, 
             alpha = 0.5) +
  geom_segment(data=gEdge,
               aes(x = from.x, 
                   xend = to.x, 
                   y = from.y, 
                   yend = to.y),
               colour="black") +
  scale_y_continuous(name = "", 
                     breaks = c(2, 4, 6, 8, 10,
                                12, 14, 16), 
                     limits=c(0, 8))  +
  scale_x_continuous(name = "", 
                     breaks = c(2, 4, 6, 8, 10,
                                12, 14, 16), 
                     limits=c(2, 12)) + 
  ggtitle('F)') +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))



fig2 <- aa + bb + cc +dd + ee + ff + plot_layout(ncol = 2)
ggsave('figures/Fig2.png', fig2, width = 10, height = 12)

