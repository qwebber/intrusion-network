
### Packages ----
libs <- c('data.table', 'igraph', 'ggraph',
          'ggplot2', 'krsp', 'sp', 'adehabitatHR',
          'sf', 'ggnetwork')
lapply(libs, require, character.only = TRUE)

## load data
df <- fread("output/spatial-locs.csv")
df <- df[grid == "KL" & year == "2016"]

## load data
edge_list <- readRDS("output/edge_list.RDS")
edge_list$gr_year <- as.factor(paste(edge_list$year, edge_list$grid, sep = "_"))

## load territory polygons
polys <- readRDS("output/edge_list_data/polygons.RDS")

gr_year <- data.table(id = names(polys))


## generate igraph objects
metrics <- c()
for(i in 1:n){ 
  
  k <- "2016_KL"#gr_year[i]
  
  df1 <- edge_list[gr_year == k][, .N, by = c("owner", "intruder")]
  
  grph <- graph.data.frame(df1, directed = T)
  
}


##
df <- df[grid == "KL" | year == 2016]
coordsMeans <- data.frame(squirrel_id = as.factor(unique(df$squirrel_id)),
                          locx = df[, mean(locx), by = "squirrel_id"]$V1,
                          locy = df[, mean(locy), by = "squirrel_id"]$V1)
setnames(coordsMeans, c("locy", "locx"), c("y", "x"))

## create layout
lay = create_layout(grph, layout = coordsMeans) # algorithm = 'kk')

ggraph(lay) + 
  geom_edge_link() + 
  geom_node_point(aes(color=factor(squirrel_id)), 
                  #size = (graph.strength(KL_2016out)+0.0125)*75,
                  alpha = 0.75) +
  scale_edge_width(range=c(0.1,2)) +
  #ggtitle("A) Winter (2007)") +
  ylab('') +
  xlab('') +
  coord_fixed() +
  #scale_color_viridis_d() +
  theme(#legend.position = 'none',
    legend.key = element_blank(),
    axis.text=element_text(size=12, color = "black"),
    axis.title=element_text(size=12),
    strip.text = element_text(size=12,face = "bold"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))




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

polys2016 <- sf::st_transform(polys[[30]])
polys2016$gr_year <- rep("2016", length(polys2016$id_polygons))

df <- df[grid == "KL" | year == 2016]
df2 <- df[squirrel_id == 12613 | squirrel_id == 19718 |
          squirrel_id == 22008 | 
          squirrel_id == 12207]

ggplot(data = #polys2016) +
         subset(polys2016,  
                     id_polygons == 12613 |
                     id_polygons == 19718 | 
                     id_polygons == 22008 | 
                     id_polygons == 12207)) +
  geom_sf(aes(fill = id_polygons), 
          alpha = 0.5) +
  geom_point(data = df2, 
             aes(locx, locy, color = factor(squirrel_id)), 
             alpha = 0.25) + 
  xlim(-10, 25) + 
  ylim(-3, 25) +
  labs("") +
  #scale_fill_viridis_d() +
  theme(#legend.position = 'none',
        legend.key = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

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

