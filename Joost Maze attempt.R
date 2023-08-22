generate_maze <- function(height, width) {
  maze <- matrix(1, nrow = height * 2 + 1, ncol = width * 2 + 1)
  
  start_row <- 1  # Set the start to the first row
  start_col <- 3  # Set the start to the third column
  maze[start_row, start_col] <- 0
  
  end_row <- height * 2 - 1  # Set the end to the last row
  end_col <- width * 2  # Set the end to the second-to-last column
  maze[end_row, end_col] <- 0
  
  stack <- list(list(row = start_row, col = start_col))
  
  while (length(stack) > 0) {
    current_cell <- stack[[length(stack)]]
    row <- current_cell$row
    col <- current_cell$col
    
    unvisited_neighbors <- list()
    
    for (dr in c(-2, 2)) {
      if (row + dr >= 1 && row + dr <= height * 2 + 1 && maze[row + dr, col] == 1) {
        unvisited_neighbors <- c(unvisited_neighbors, list(list(row = row + dr, col = col)))
      }
    }
    
    for (dc in c(-2, 2)) {
      if (col + dc >= 1 && col + dc <= width * 2 + 1 && maze[row, col + dc] == 1) {
        unvisited_neighbors <- c(unvisited_neighbors, list(list(row = row, col = col + dc)))
      }
    }
    
    if (length(unvisited_neighbors) > 0) {
      next_cell <- sample(unvisited_neighbors, 1)
      next_row <- next_cell[[1]]$row
      next_col <- next_cell[[1]]$col
      
      maze[next_row, next_col] <- 0
      maze[row + (next_row - row) / 2, col + (next_col - col) / 2] <- 0
      
      stack <- c(stack, list(next_cell[[1]]))
    } else {
      stack <- stack[-length(stack)]
    }
  }
  
  return(maze)
}

# Input maze dimensions
height <- 10
width <- 10

# Generate maze
maze <- generate_maze(height, width)

# Set colors for starting and ending points
start_row <- 1  # Define start_row here
start_col <- 3  # Define start_col here
end_row <- height * 2 + 1  # Define end_row here
end_col <- width * 2 - 1  # Define end_col here

# Create a custom plotting function with colors
plot_maze_with_colors <- function(maze, start_row, start_col, end_row, end_col) {
  for (row in 1:nrow(maze)) {
    for (col in 1:ncol(maze)) {
      if (maze[row, col] == 0) {
        color <- "black"
      } else if (row == start_row && col == start_col) {
        color <- "green"
      } else if (row == end_row && col == end_col) {
        color <- "red"
      } else {
        color <- "white"
      }
      rect(col - 1, nrow(maze) - row, col, nrow(maze) - row + 1, col = color)
    }
  }
}

# Create a plot for the maze with colors
plot(0:ncol(maze), 0:nrow(maze), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1)
plot_maze_with_colors(maze, start_row, start_col, end_row, end_col)
