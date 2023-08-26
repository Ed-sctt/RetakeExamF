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
#'Example_maze <- RetakeExamF::myGraph (nrows = 5,ncols = 9) 
#'Example_maze <- RetakeExamF::dfs_method (GraphMade = Example_maze)
#'RetakeExamF::solverGraph(Example_maze,nrows= 5,ncols = 5)

#'@export
solverGraph <-function(GraphMade = NA, nrows=0, ncols=0){
  
  #Check if it's still the same graph with same dimensions.
  if (nrows <= 0 | ncols <= 0)
    stop("ERROR! Wrong height,width seize")
  
  # Creation of subgraph with no walls
  
  graph_i <- igraph::subgraph.edges(GraphMade, igraph::E(GraphMade)[igraph::E(GraphMade)$wall == "OFF"], delete.vertices = FALSE)
  
  # Construction of shortest path algo
  
  short_path <- igraph::shortest_paths(graph_i, igraph::V(graph_i)[1], igraph::V(graph_i)[igraph::vcount(graph_i)], mode="all", output="vpath")
  
  # Creation of data.frame with the coordinates of the edges and vertices
  df <- data.frame(x1 = c(0, 1, 0, ncols), y1 = c(0, 0, nrows, 0), x2 = c(0, ncols, ncols-1, ncols), y2 = c(nrows,0, nrows, nrows))
  df_solution <- data.frame(x1 = c(), y1 = c(), x2 = c(), y2 = c())
  
  # loop within a loop to check all the interactions between edges and vertices
  for (r in 1:nrows)
    for (c in 1:ncols){
      
      if (paste("#", as.character(r), as.character(c), sep="_") %in% igraph::as_ids(short_path$vpath[[1]]))
        df_solution <- rbind(df_solution, data.frame(x1 = (c-1), y1 = (r-1), x2 = c, y2 = r))
      
      if (r < nrows)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r+1), as.character(c), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = (c-1), y1 = r, x2 = c, y2 = r)) # %--% means undirected edges defined in igraph
      
      
      if (c < ncols)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r), as.character(c+1), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = c, y1 = (r-1), x2 = c, y2 = r))
    }
  
  # plot the path solution on the graph
  
  k <- ggplot2::ggplot(data = df)+
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2))+
    ggplot2::theme_void()+
    ggplot2::geom_rect(data=df_solution, ggplot2::aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="green", alpha=0.4)
  return(k)
}
