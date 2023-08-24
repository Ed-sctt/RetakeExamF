#'@title solverGraph function
#'@author Joost and Edward
#'@details
#'The function transforms the graph into a data frame and then 
#'creates a plot of the existing maze with ggplot2 package.
#'
#'@param GraphMade An existing graph 
#'@param nrows number of rows, maze height value arbitrarly set to 0.
#'@param ncols number of cols, maze weight value arbitrarly set to 0.

#'@return The function returns returns a green path indicating the solution of the maze.

#'@examples
#'Example_maze <- myGraph (nrows = 5,ncols = 9) 
#'Example_maze <- dfs_method (GraphMade = Example_maze)
#'solverGraph(Example_maze,nrows= 5,ncols = 5)

#'@export
solverGraph <-function(GraphMade = NA, nrows=0, ncols=0){
  
  #Check if it's still the same graph with same dimensions.
  if (nrows <= 0 | ncols <= 0)
    stop("ERROR! Wrong height,width seize")
  
  # First, we need to create a subgraph that contains only edges
  # with no walls (i.e., cells where a path between two cells exist)
  graph_i <- igraph::subgraph.edges(GraphMade, igraph::E(GraphMade)[igraph::E(GraphMade)$wall == "OFF"], delete.vertices = FALSE)
  
  # Now, we will find the maze solution - it will be the shortest path between the start end end maze cells
  sp_start_end <- igraph::shortest_paths(graph_i, igraph::V(graph_i)[1], igraph::V(graph_i)[igraph::vcount(graph_i)], mode="all", output="vpath")
  
  # Finally, we plot the maze and its solution
  
  # Add coordinates for the outside box
  # TBD - incoorporate start and end cells, including checking if it is outside cell
  df <- data.frame(x1 = c(0, 1, 0, ncols), y1 = c(0, 0, nrows, 0), x2 = c(0, ncols, ncols-1, ncols), y2 = c(nrows,0, nrows, nrows))
  df_solution <- data.frame(x1 = c(), y1 = c(), x2 = c(), y2 = c())
  
  for (r in 1:nrows)
    for (c in 1:ncols){
      
      if (paste("#", as.character(r), as.character(c), sep="_") %in% igraph::as_ids(sp_start_end$vpath[[1]]))
        df_solution <- rbind(df_solution, data.frame(x1 = (c-1), y1 = (r-1), x2 = c, y2 = r))
      
      if (r < nrows)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r+1), as.character(c), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = (c-1), y1 = r, x2 = c, y2 = r)) # %--% means undirected edges defined in igraph
      
      
      if (c < ncols)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r), as.character(c+1), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = c, y1 = (r-1), x2 = c, y2 = r))
    }
  
  k <- ggplot2::ggplot(data = df)+
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2))+
    ggplot2::theme_void()+
    ggplot2::geom_rect(data=df_solution, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="green", alpha=0.4)
  return(k)
}
