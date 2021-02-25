





library(ggraph)

coordsMeans <- data.frame(squirrel_id = unique(df$squirrel_id),
                          locy = df[, median(locy), by = "squirrel_id"]$V1,
                          locx = df[, median(locx), by = "squirrel_id"]$V1)
ids <- df[, .N, by = c("squirrel_id")][,c("N") := NULL]
coordsMeans <- merge(ids, coordsMeans, by = "squirrel_id")
KL2016 = create_layout(grph, layout = coordsMeans) # algorithm = 'kk')



ggraph(grph) + 
  geom_edge_link() + 
  geom_node_point(#size = (graph.strength(grph)*0.1),
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

