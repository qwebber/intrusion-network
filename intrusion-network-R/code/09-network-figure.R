
### Packages ----
libs <- c('data.table', 'igraph', 'ggraph',
          'ggplot2', 'krsp', 'sp', 'adehabitatHR',
          'sf', 'ggnetwork', 'dils')
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


df1 <- edge_list[gr_year == "2018_KL"][, .N, by = c("owner", "intruder", "Nintruder")]

df2 <- data.table(owner = df1$owner,
                  intruder = df1$intruder, 
                  TI = df1$N/(df1$N + df1$Nintruder))

adj <- AdjacencyFromEdgelist(df2)

diag(adj$adjacency) <- 0

g <- graph_from_adjacency_matrix(adj$adjacency, weighted = TRUE, mode = c("directed")) 

##
df10 <- df[grid == "KL" & year == 2018]
coordsMeans <- data.frame(squirrel_id = as.factor(unique(df10$squirrel_id)),
                          locx = df10[, mean(locx)/30, by = "squirrel_id"]$V1,
                          locy = df10[, mean(locy)/30, by = "squirrel_id"]$V1)
setnames(coordsMeans, c("locy", "locx"), c("y", "x"))

coordsMeans <- merge(coordsMeans, flastall[,c("squirrel_id", "sex")], by = "squirrel_id")

## create layout
lay = create_layout(g, layout = coordsMeans) # algorithm = 'kk')

polys2018 <- sf::st_transform(polys$KL_2018)
polys2018$gr_year <- rep("2018", length(polys2018$id_polygons))


png("figures/Fig-pts-hr-net.png", width = 8000, height = 8000, units = "px", res = 600)
### Plot points 
aa <- ggplot(df[gr_year == "KL_2018"]) +
  geom_jitter(aes(locx/30, locy/30, color = factor(squirrel_id)), 
             alpha = 0.25) + 
  ylab("") + xlab("") +
  ggtitle("A)") +
  geom_rect(aes(xmin = -8, xmax = 0, 
                ymin = 8, ymax = 16), color = "black", fill = "transparent") +
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

bb <- ggplot(df[gr_year == "KL_2018"]) +
  geom_jitter(aes(locx/30, locy/30, color = factor(squirrel_id)), 
              alpha = 0.25) + 
  ylab("") + xlab("") +
  ggtitle("B)") +
  ylim(8, 16) +
  xlim(-8, 0) +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

ggplot(data = polys2018) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  coord_sf(datum = st_crs(32648)) +
  geom_rect(aes(xmin = -8*30, xmax = 0, 
                ymin = 8*30, ymax = 16*30), color = "black", fill = "transparent") +
  
  scale_x_continuous(labels = c(-8, -4, 0, 4, 8, 12, 16), 
                     breaks = c(6363600,
                                6363400,
                                6363200,
                                6363000,
                                6362800,
                                6362600,
                                6362400),
                     limits = c(-12*30, 20*30)) + 
  ggtitle('C)') +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text= element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

dd <- ggplot(data = polys2018) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  coord_sf(datum = st_crs(32648)) +
  scale_y_continuous(labels = c(9, 10, 11, 12, 13, 14, 15), 
                     breaks = c(19995550, 
                                19995500,
                                19995450,
                                19995400,
                                19995350,
                                19995300,
                                19995250),
                     limits = c(8*30, 16*30)) +
  scale_x_continuous(labels = c(-8, -7, -6, -5, -4, -3, -2), 
                     breaks = c(6363600,
                                6363550,
                                6363500,
                                6363450,
                                6363400,
                                6363350,
                                6363300),
                     limits = c(-8*30, 0)) +
  ggtitle('D)') +
  scale_fill_viridis_d() +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        axis.text= element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.ticks = element_line(),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

### plot ggraph ###
ee <- ggraph(lay) + 
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(1, 'mm'), 
                 alpha = 0.5) + 
  geom_node_point(aes(color=factor(squirrel_id)), 
                  size = degree(g)/5,
                  alpha = 0.75) +
  geom_rect(aes(xmin = -8, xmax = 0, 
                ymin = 8, ymax = 16), color = "black", fill = "transparent") +
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

ff <- ggraph(lay) + 
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')),
                 end_cap = circle(1, 'mm'), 
                 alpha = 0.5) + 
  geom_node_point(aes(color=factor(squirrel_id)), 
                  size = degree(g)/5,
                  alpha = 0.75) +
  scale_y_continuous(name = "", 
                     breaks = c(8, 10,
                                12, 14, 16), 
                     limits=c(8, 16))  +
  scale_x_continuous(name = "", 
                     breaks = c(-8, -6,
                                -4, -2, 0), 
                     limits=c(-8, 0)) + 
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



grid.arrange(aa, bb, 
             cc, dd,
             ee, ff,
             ncol = 2, nrow = 3)
dev.off()


l <- as.matrix(coordsMeans[,2:3])


plot.igraph(simplify(KL_2016out), 
  #delete.vertices(simplify(KL_2016out), degree(KL_2016out)==0),
     #rescale = FALSE,
     #edge.color= edge_color,
     #edge.width = E(KL_2016out)$Weight5,
     vertex.size = 7.5,
     vertex.color = adjustcolor("blue", alpha.f = 0.5), 
     main="KL 2016",
     layout=l,
     #vertex.label = NA,
     edge.curved=TRUE,
     edge.arrow.size=0.2,
     edge.color = adjustcolor("black", alpha.f = 0.5),
     #vertex.label.dist=0, 
     vertex.label.degree=pi/2)

setnames(coordsMeans, c("locy", "locx"), c("y", "x"))

KL2016 = create_layout(KL_2016out, layout = coordsMeans) # algorithm = 'kk')

ggplot(coordsMeans) +
  geom_point(aes(x, y, color = factor(squirrel_id)))


ggplot(KL2016) +
  geom_node_point(aes(color = factor(squirrel_id)),
                  #size = (log(graph.strength(KL_2016out, mode = c("out")))), 
                  alpha = 0.5) +
  geom_edge_link(alpha = 0.5) +
  xlim(-10, 20) + 
  ylim(0, 20) +
  theme(#legend.position = 'none',
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = margin(1, 1, 1, 1, "cm"))
grid.arrange(aa,bb, nrow = 1)



lay2 <- layout.norm(as.matrix(coordsMeans[,2:3])) 

#E(g)$width=degree(g )/5
plot.igraph(g ,
            layout=lay2,
            vertex.label.color='black',
            vertex.size = 5,
            edge.arrow.size=0.3,
            edge.color="black",
            vertex.frame.color='black',
            vertex.color="grey",
            vertex.label=NA)
box(lty = 1, col = 'black')
