library(assertthat)
library(igraph)
library(purrr)

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


make_homophily_network <- function(group_sizes = c(3, 7), mean_degree = 6,
                                   homophily = 0.0, directed = FALSE, 
                                   group_names = NULL) {
  
  N <- sum(group_sizes)
  
  assert_that(mean_degree < N, msg = "Mean degree can be at most N - 1")
  
  assert_that((-1 <= homophily) && (homophily <= 1), 
              msg = "Homophily must be between -1 and 1")
  
  g <- make_empty_graph(N, directed)
  
  if (is.null(group_names)) {
    group_names <- map_vec(1:length(group_sizes), \(ii) { as.factor(ii) })
  }
  
  a_idx = 1
  g_idx = 1
  for (group_size in group_sizes) {
    final_a_idx <- a_idx + group_size - 1
    V(g)[a_idx:final_a_idx]$group <- group_names[g_idx]
    g_idx <- g_idx + 1
    a_idx <- final_a_idx + 1
  }
  
  n_edges_per_group <- (group_sizes * mean_degree)
  print(n_edges_per_group)
  g_idx <- 1
  for (n_edges in n_edges_per_group) {
    print(n_edges)
    n_within_group <- n_edges * ((1 + homophily)/2) / 2
    n_outside_group <- round(n_edges * ((1 - homophily)/2) / length(group_sizes))

    in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
    out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    
    for (e_idx in 1:n_within_group) {
      edge_exists <- TRUE 
      while (edge_exists) {
        edge_vs <- sample(in_vertices, 2, replace = FALSE)

        edge_exists <- are_adjacent(g, edge_vs[1], edge_vs[2])
      }
      
      g <- add_edges(g, edge_vs)
      
      # Need to re-fetch these since the graph "changed".
      in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
    }
    out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    
    # Add n_outside_group edges to a random member of another group.
    for (e_idx in 1:n_outside_group) {
      
      # Find a new in-group to out-group edge.
      edge_exists <- TRUE
      while (edge_exists) {
        v1 <- sample(in_vertices, 1)
        v2 <- sample(out_vertices, 1)
        edge_exists <- are_adjacent(g, v1, v2)
      }
      
      # Add the in-group to out-group edge.
      g <- add_edges(g, c(v1, v2))
      
      # Need to re-fetch these since the graph "changed".
      in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
      out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    }
    
    g_idx <- g_idx + 1
  }
  
  return (g)
}

ggnetplot <- function(g, ...) {
  
  return(
    ggplot(ggnetwork(g, layout = layout_in_circle(g)), 
           aes(x=x, y=y, xend=xend, yend=yend, ...))
  )
}

add_homophilous_edges <- function(vert, network, mean_degree = 6) {
  
}

plot_homopily_network <- function(hnet) {

}


make_minmaj_network <- function(N, frac_minority, hmin = 0.0, hmaj = 0.0) {
  
}



