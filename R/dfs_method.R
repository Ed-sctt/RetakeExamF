#' @title dfs_method function
#' @author Joost and Edward
#' @description
#' Generate a maze using a randomized adaptation of the depth-first search (DFS) algorithm
#'
#' @param GraphMade an existing graph object. Value is set to NA
#' @return The function returns a maze built according to dfs algorithm with paths and walls displayed.
#'
#' @examples
#'Example_maze <- RetakeExamF::myGraph (nrows = 5,ncols = 9) 
#'Example_maze <- RetakeExamF::dfs_method (GraphMade = Example_maze)
#'RetakeExamF::solverGraph(Example_maze,nrows= 5,ncols = 5)

#' 
#' @export


dfs_method <- function(GraphMade= NA){
  
  # Create empty node stack vector
  stack <- c()
  Done <- FALSE
  
 
  # Initial node defined as #_1_1
  start_node = "#_1_1"
  
  while(!Done){
    
    # equal 1 if node visited
    igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == start_node )]$visited <- 1
    
    # check adjacent nodes
    v_adjacent <- igraph::neighbors(GraphMade, igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == start_node )], "all")
    
    # check all non visited nodes
    index_adjacent_unvisited <- which(igraph::V(GraphMade)[igraph::V(GraphMade)[v_adjacent]]$visited == 0)
    
    # unvisited nodes part
    if (length(index_adjacent_unvisited) > 0){
      
      # pick one randomly
      randomly_select <- index_adjacent_unvisited[sample(1:length(index_adjacent_unvisited), 1)]
      
      # remove wall between the 2 nodes
      igraph::E(GraphMade)[start_node  %--% v_adjacent[randomly_select]$name]$wall <- "OFF"
      
      # record it into stak vector
      stack <- c(start_node, stack)
      
      # assign new node as the adjacent
      start_node <- v_adjacent[randomly_select]$name

    } else {
      # when node is finished
      igraph::V(GraphMade)[which(igraph::V(GraphMade)$name == start_node )]$visited <- 2
      
      # If stack is not empty, get an element
      if (length(stack) > 0){
        start_node <- stack[1]
        
       stack <- stack[-1]
        
        
      } else {
        
        # If stack is empty, check if other univisited cnodes
        choose_from <- igraph::V(GraphMade)[which(igraph::V(GraphMade)$visited == 0)]$name
        
        # If yes, select one node randomly
        if (length(choose_from) > 0){
          start_node <- choose_from[sample(1:length(choose_from), 1)]
        } else {
          # If not, we are done
          Done <- TRUE
        }
      }
    }
  }
  GraphMade
}

