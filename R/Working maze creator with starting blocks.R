library(ggplot2)

# Initialize maze dimensions with walls
width <- 28
height <- 6

# Add +1 to dimensions if even to ensure an outer wall
maze <- matrix(1, nrow = height + (height %% 2 == 0), ncol = width + (width %% 2 == 0))

# Initialize stack for backtracking
stack <- list(start = c(2, 2))

# Start from an arbitrary point and mark it as visited (carve it out)
current <- stack$start
maze[current[1], current[2]] <- 0

# Helper function to check unvisited neighbors
check_neighbors <- function(cell, maze) {
  x <- cell[1]
  y <- cell[2]
  neighbors <- list()
  
  # Check each direction
  directions <- list("up" = c(x - 2, y), "down" = c(x + 2, y),
                     "left" = c(x, y - 2), "right" = c(x, y + 2))
  
  for (dir in names(directions)) {
    next_cell <- directions[[dir]]
    nx <- next_cell[1]
    ny <- next_cell[2]
    
    if (nx >= 1 && nx <= nrow(maze) && ny >= 1 && ny <= ncol(maze)) {
      if (maze[nx, ny] == 1) {
        neighbors <- append(neighbors, list(list(cell = next_cell, direction = dir)))
      }
    }
  }
  
  return(neighbors)
}

# Recursive backtracker
while (length(stack) > 0) {
  current <- stack[[length(stack)]]
  neighbors <- check_neighbors(current, maze)
  
  if (length(neighbors) > 0) {
    index <- sample(1:length(neighbors), 1)
    chosen <- neighbors[[index]]
    
    new_cell <- chosen$cell
    new_x <- new_cell[1]
    new_y <- new_cell[2]
    
    # Mark the cell and the wall leading to it as visited (carve them out)
    maze[new_x, new_y] <- 0
    maze[(current[1] + new_x) / 2, (current[2] + new_y) / 2] <- 0
    
    # Push to stack
    stack <- append(stack, list(new_cell))
  } else {
    # Pop from stack
    stack <- stack[-length(stack)]
  }
}

# Visualization
maze_df <- expand.grid(x = 1:ncol(maze), y = 1:nrow(maze))
maze_df$cell_value <- as.vector(t(maze))

# Add a column for cell color
maze_df$cell_color <- "white"
maze_df$cell_color[maze_df$cell_value == 1] <- "black"

# Mark the start cell green and the exit cell red
maze_df$cell_color[maze_df$x == 2 & maze_df$y == 2] <- "green"
maze_df$cell_color[maze_df$x == ncol(maze) - 1 & maze_df$y == nrow(maze) - 1] <- "red"

# Generate the plot
ggplot(maze_df, aes(x = x, y = y)) +
  geom_tile(aes(fill = cell_color), color = "white") +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()
