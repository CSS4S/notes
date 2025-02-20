library(shiny)
library(igraph)
library(ggplot2)
library(GGally)
library(assertthat)

make_homophily_network <- function(N = 10, group_sizes = c(3, 7), mean_degree = 6,
                                   homophily = 0.0, directed = FALSE, 
                                   group_names = NULL) {
  assert_that(mean_degree < N, msg = "Mean degree can be at most N - 1")
  assert_that((-1 <= homophily) && (homophily <= 1), 
              msg = "Homophily must be between -1 and 1")
  
  g <- make_empty_graph(N, directed)
  
  if (is.null(group_names)) {
    group_names <- as.character(seq_along(group_sizes))
  }
  
  a_idx = 1
  g_idx = 1
  for (group_size in group_sizes) {
    final_a_idx <- a_idx + group_size - 1
    V(g)[a_idx:final_a_idx]$group <- group_names[g_idx]
    g_idx <- g_idx + 1
    a_idx <- final_a_idx + 1
  }
  
  n_edges_per_group <- group_sizes * mean_degree
  g_idx <- 1
  for (n_edges in n_edges_per_group) {
    n_within_group <- n_edges * ((1 + homophily)/2)
    n_outside_group <- n_edges * ((1 - homophily)/2)
    
    in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
    out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    
    for (e_idx in 1:n_within_group) {
      edge_exists <- TRUE 
      while (edge_exists) {
        edge_vs <- sample(in_vertices, 2, replace = FALSE)
        edge_exists <- are_adjacent(g, edge_vs[1], edge_vs[2])
      }
      g <- add_edges(g, edge_vs)
      in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
      out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    }
    
    for (e_idx in 1:n_outside_group) {
      edge_exists <- TRUE
      while (edge_exists) {
        v1 <- sample(in_vertices, 1)
        v2 <- sample(out_vertices, 1)
        edge_exists <- are_adjacent(g, v1, v2)
      }
      g <- add_edges(g, c(v1, v2))
      in_vertices <- V(g)[V(g)$group == group_names[g_idx]]
      out_vertices <- V(g)[V(g)$group != group_names[g_idx]]
    }
    
    g_idx <- g_idx + 1
  }
  
  return(g)
}

ggnetplot <- function(g) {
  ggplot(ggnetwork(g, layout = layout_in_circle(g)), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = 'gray') +
    geom_nodes(aes(color = factor(group)), size = 6) +
    theme_void() +
    scale_color_brewer(palette = "Set1")
}

ui <- fluidPage(
  titlePanel("Homophilic Network Visualization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Number of Nodes:", 10, min = 2),
      textInput("group_sizes", "Group Sizes (comma-separated):", "3,7"),
      numericInput("mean_degree", "Mean Degree:", 6, min = 1),
      sliderInput("homophily", "Homophily:", min = -1, max = 1, value = 0, step = 0.1),
      actionButton("generate", "Generate Network")
    ),
    mainPanel(
      plotOutput("networkPlot")
    )
  )
)

server <- function(input, output) {
  network <- reactive({
    input$generate
    isolate({
      group_sizes <- as.numeric(strsplit(input$group_sizes, ",")[[1]])
      make_homophily_network(
        N = input$N,
        group_sizes = group_sizes,
        mean_degree = input$mean_degree,
        homophily = input$homophily
      )
    })
  })
  
  output$networkPlot <- renderPlot({
    req(input$generate)
    g <- network()
    ggnetplot(g)
  })
}

shinyApp(ui = ui, server = server)


# Run the application 
shinyApp(ui = ui, server = server)
