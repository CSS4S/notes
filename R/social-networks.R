plot_florentine_default <- function() {

  # The Florentine network data contains one disconnected family; remove it here.
  florentine_m <- delete_vertices(florentine_m, which(degree(florentine_m) == 0))

  # Convert to ggnetwork format for plotting.
  flornet <- ggnetwork(florentine_m)

  # The x, y, xend, and yend variables are assigned in ggnetwork call.
  ggplot(flornet, aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(color = "black") +
      geom_nodes(aes(size = wealth), color = "darkgrey") + 
      geom_nodelabel_repel(aes(label = name), color = "#0099b7", 
                           box.padding = unit(0.5, "lines")) +
      theme_blank()
}
