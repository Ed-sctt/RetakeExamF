#' @title myGraph function
#' @author Joost and Edward
#' @details
#' The function creates an undirected network with edges and nodes. 
#' These elements are translated to 'walls' are ON to build the maze. 

#' @param nrows number of rows, maze height value arbitrarly set to 0.
#' @param ncols number of cols, maze weight value arbitrarly set to 0.
#'
#' @return The function returns an undirected graph network with nodes and edges according to the nrows and ncols defined

#' @examples
#' Example_maze <- RetakeExamF::myGraph (nrows = 5,ncols = 9) 
#' Example_maze <- RetakeExamF::dfs_method (GraphMade = Example_maze)
#' RetakeExamF::plotmyMaze(Example_maze,nrows= 5,ncols = 5)

#' @export
myGraph <- function(nrows=0, ncols=0){

  if (nrows <= 0 | ncols <= 0)
    stop("ERROR ! Wrong height,width seize ")

  # Create list of edges
  df <- data.frame(A=c(), B=c())

  for (i in 1:nrows) # row number
    for (j in 1:ncols){ # col number
      
      if (i < nrows)
        df<- rbind(df,
                   data.frame(A = paste("#", as.character(i), as.character(j), sep="_"),
                              B = paste("#", as.character(i+1), as.character(j), sep ="_"))) 
      
      if (j < ncols)
        df <- rbind(df,
                        data.frame(A = paste("#", as.character(i), as.character(j), sep="_"),
                                   B = paste("#", as.character(i), as.character(j+1), sep="_")))
    }
  
  # Create undirected graph from df
  GraphMade <- igraph::simplify(igraph::graph_from_data_frame(df, directed=FALSE))
  
  # define attribute walls 
  GraphMade <- igraph::set_edge_attr(GraphMade , "wall", value = "ON")
  
  # Set nodes, vertices attributes 
  GraphMade <- igraph::set_vertex_attr(GraphMade , "visited", value = 0)

  # Return the graph
  return(GraphMade)
}

