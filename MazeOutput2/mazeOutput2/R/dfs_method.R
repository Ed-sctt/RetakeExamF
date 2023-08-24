#' @title dfs_method function
#' @author Joost and Edward
#' @description
#' Generate a maze using a randomized adaptation of the depth-first search (DFS) algorithm
#'
#' @param GraphMade an existing maze graph object. Value is set to NA
#' @return The function returns a maze built according to dfs algorithm with paths anf walls displayed.
#'
#' @examples
#' Example_maze <- myGraph(nrows = 5,ncols = 9) 
#' Example_maze <- dfs_method(GraphMade = Example_maze)
#' plotmyMaze(Example_maze,nrows= 5,ncols = 5)
#' 
#' @export


dfs_method <- function(GraphMade= NA){
  
  # Create empty node stack vector
  stack <- c()
  Done <- FALSE
  
  # Select the start cell/node the current and mark it as visited
  # We will use cell 1-1 as a start cell. This can easily be changed to allow other cells to be start cells
  
  current_cell_name = "#_1_1"
  
  while(!Done){
    
    # Mark this cell/node as visited (but not finished)
    igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == current_cell_name )]$visited <- 1
    
    # Find all adjacent cells/nodes to start node
    v_adjacent <- igraph::neighbors(GraphMade, igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == current_cell_name )], "all")
    
    # Find all adjacent nodes that were not visited before
    index_adjacent_unvisited <- which(igraph::V(GraphMade)[igraph::V(GraphMade)[v_adjacent]]$visited == 0)
    
    # If such nodes exist
    if (length(index_adjacent_unvisited) > 0){
      # Randomly select one of the unvisited neighbors
      randomly_select <- index_adjacent_unvisited[sample(1:length(index_adjacent_unvisited), 1)]
      
      # Remove the wall between the current cell and the chosen cell
      igraph::E(GraphMade)[current_cell_name %--% v_adjacent[randomly_select]$name]$wall <- "OFF"
      
      # Push the current cell/node to the stack
      stack <- c(current_cell_name, stack)
      
      # Make the adjacent node the current node
      current_cell_name <- v_adjacent[randomly_select]$name

    } else {
      # Mark the cell as finished
      igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == current_cell_name )]$visited <- 2
      
      # If stack is not empty, get an element
      if (length(stack) > 0){
        current_cell_name <- stack[1]
        
       stack <- stack[-1]
        
        
      } else {
        
        # If stack is empty, check if there are any other univisited cells/nodes
        choose_from <- igraph::V(GraphMade)[which(igraph::V(GraphMade)$visited == 0)]$name
        
        # If yes, select one node randomly
        if (length(choose_from) > 0){
          current_cell_name <- choose_from[sample(1:length(choose_from), 1)]
        } else {
          # If not, we are done
          Done <- TRUE
        }
      }
    }
  }
  GraphMade
}


