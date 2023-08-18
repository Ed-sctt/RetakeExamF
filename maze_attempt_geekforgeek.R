# Node class definition
Node <- setRefClass('Node',
                    fields = list(val = 'numeric', next_element = 'Node'))
node1 <- Node()# IMPORTANT to create an object and call the methods

# Stack class definition
StackClass <- setRefClass('Stack', fields = list(
  head = 'Node',
  length = 'numeric'
),
methods = list(
  insert = function(data) {
    newNode <- Node$new()  # Create a new Node object
    newNode$val <- data
    newNode$next_element <- stackcls$head
    stackcls$head <- newNode
    stackcls$length <- stackcls$length + 1
  },
  pop = function() {
    if (stackcls$length == 0) {
      return(NULL)
    } else {
      returned <- stackcls$head$val
      stackcls$head <- stackcls$head$next_element
      stackcls$length <- stackcls$length - 1
      return(returned)
    }
  },
  not_empty = function() {
    return(stackcls$length > 0)
  },
  top = function() {
    return(stackcls$head$val)
  },
  test = function(){
    print('Edward')
  }
)
)
stackcls<-StackClass() # IMPORTANT to create an object and call the methods

# Function to generate the random maze
random_maze_generator <- function(r, c, P0, P1) {
  ROWS <- r
  COLS <- c
  
  # Array with only walls (where paths will be created)
  maze <- matrix(0, nrow = ROWS, ncol = COLS)
  
  # Auxiliary matrices to avoid cycles
  seen <- matrix(FALSE, nrow = ROWS, ncol = COLS)
  previous <- array(list(c(-1, -1)), dim = c(ROWS, COLS))
  
  S <- stackcls # create a new instance of class StackClass
  
  # Insert initial position
  S$insert(P0)
  
  # Keep walking on the graph using dfs
  # until we have no more paths to traverse
  # (create)
  
  while (S$not_empty()) {
    
    # Remove the position of the Stack
    # and mark it as seen
    pos <- S$pop()
    x <- pos[[1]]
    y <- pos[[2]]
    seen[x + 1, y + 1] <- TRUE
    
    # Check if it will create a cycle
    # if the adjacent position is valid
    # (is in the maze) and the position
    # is not already marked as a path
    # (was traversed during the dfs) and
    # this position is not the one before it
    # in the dfs path it means that
    # the current position must not be marked.
    if ((x + 1 < ROWS) && (maze[x + 1, y + 1] == 1) &&
        (identical(previous[[x + 1, y + 1]], c(x + 1, y)))) {
      next
    }
    if ((x > 0) && (maze[x, y + 1] == 1) &&
        (identical(previous[[x - 1, y + 1]], c(x - 1, y)))) {
      next
    }
    if ((y + 1 < COLS) && (maze[x + 1, y + 2] == 1) &&
        (identical(previous[[x + 1, y + 1]], c(x, y + 1)))) {
      next
    }
    if ((y > 0) && (maze[x + 1, y] == 1) &&
        (identical(previous[[x + 1, y - 1]], c(x, y - 1)))) {
      next
    }
    
    # Mark as walkable position
    maze[x + 1, y + 1] <- 1
    
    # Initialize the to_stack list
    to_stack <- list()
    
    # Before inserting any position,
    # check if it is in the boundaries of
    # the maze and if it was seen (to avoid cycles)
    
    # If adj position is valid and was not seen yet
    if ((x + 2 <= ROWS) && (!seen[x + 1, y + 1])) {
      # Mark the adj position as seen
      seen[x + 2, y + 1] <- TRUE
      
      # Memorize the position to insert the
      # position in the stack
      to_stack <- c(to_stack, list(c(x + 1, y)))
      
      # Memorize the current position as its
      # previous position on the path
      previous[[x + 2, y + 1]] <- c(x + 1, y)
    }
    
    if ((x >= 2) && (!seen[x, y + 1])) {
      # Mark the adj position as seen
      seen[x, y + 1] <- TRUE
      
      # Memorize the position to insert the
      # position in the stack
      to_stack <- c(to_stack, list(c(x - 1, y)))
      
      # Memorize the current position as its
      # previous position on the path
      previous[[x, y + 1]] <- c(x - 1, y)
    }
    
    if ((y + 2 <= COLS) && (!seen[x + 1, y + 2])) {
      # Mark the adj position as seen
      seen[x + 1, y + 2] <- TRUE
      
      # Memorize the position to insert the
      # position in the stack
      to_stack <- c(to_stack, list(c(x, y + 1)))
      
      # Memorize the current position as its
      # previous position on the path
      previous[[x + 1, y + 2]] <- c(x, y + 1)
    }
    
    if ((y >= 2) && (!seen[x + 1, y])) {
      # Mark the adj position as seen
      seen[x + 1, y] <- TRUE
      
      # Memorize the position to insert the
      # position in the stack
      to_stack <- c(to_stack, list(c(x, y - 1)))
      
      # Memorize the current position as its
      # previous position on the path
      previous[[x + 1, y]] <- c(x, y - 1)
    }
    # Indicates if Pf is a neighbour position
    pf_flag <- FALSE
    while (length(to_stack) > 0) {
      neighbour <- to_stack[[length(to_stack)]]
      to_stack <- to_stack[1:(length(to_stack) - 1)]
      # ... Insert neighbour into the stack
    }
    
    # This way, Pf will be on the top
    if (pf_flag) {
      S$insert(P1)
    }
  }
  
  # Mark the initial and final positions
  x0 <- P0[[1]]
  y0 <- P0[[2]]
  xf <- P1[[1]]
  yf <- P1[[2]]
  maze[x0 + 1, y0 + 1] <- 2
  maze[xf + 1, yf + 1] <- 3
  
  # Return maze formed by the traversed path
  return(maze)
}

# Driver code
N <- 5
M <- 5
P0 <- c(0, 0)
P1 <- c(4, 4)
maze <- random_maze_generator(N, M, P0, P1)

# Display the maze
for (i in 1:N) {
  cat(paste(maze[i, ], collapse = " "), "\n")
}
