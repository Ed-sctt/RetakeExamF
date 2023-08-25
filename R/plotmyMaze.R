#' @title ploymyMaze function
#' @author Joost and Edward
#' @details
#' The function transforms the graph into a data frame and then 
#' creates a plot of the existing maze with ggplot2 package.

#' @param GraphMade An existing graph 
#' @param nrows number of rows, maze height value arbitrarly set to 0.
#' @param ncols number of cols, maze weight value arbitrarly set to 0.

#' @return The function returns a plot of the existing maze in 2D. 
#' @name plotmyMaze
#' @examples
#' Example_maze <- myGraph (nrows = 5,ncols = 9) 
#' Example_maze <- dfs_method (GraphMade = Example_maze)
#' plotmyMaze(Example_maze,nrows= 5,ncols = 5)

# Definied global funcitons, variables
utils::globalVariables(c("df", "x1", "x2",'y1','y2'))
utils::globalVariables(c('%--%'))

#' @export
plotmyMaze <-function(GraphMade=NA, nrows=0, ncols=0){
  
  if (nrows <= 0 | ncols <= 0)
    stop("ERROR")
  
  # Add coordinates for the outside box
  # TBD - incoorporate start and end cells, including checking if it is outside cell
  df <- data.frame(x1 = c(0, 1, 0, ncols), y1 = c(0, 0, nrows, 0), x2 = c(0, ncols, ncols-1, ncols), y2 = c(nrows,0, nrows, nrows))
  
  for (r in 1:nrows)
    for (c in 1:ncols){
      
      if (r < nrows)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r+1), as.character(c), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = (c-1), y1 = r, x2 = c, y2 = r)) # %--% means undirected edges defined in igraph
      
      
      if (c < ncols)
        if (igraph::E(GraphMade)[paste("#", as.character(r), as.character(c), sep="_") %--% paste("#", as.character(r), as.character(c+1), sep="_")]$wall == "ON")
          df <- rbind(df, data.frame(x1 = c, y1 = (r-1), x2 = c, y2 = r))
    }
  
  ploti <-ggplot2::ggplot(data=df)+
    ggplot2::geom_segment(ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2))+
    ggplot2::theme_void() # Add more aesthetics if time 
  return(ploti)
}


