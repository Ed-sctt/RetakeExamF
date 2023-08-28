add_outer_wall <- function(maze_df, width, height) {
  maze_df$cell_color[maze_df$x == 1 | maze_df$x == width] <- "black"
  maze_df$cell_color[maze_df$y == 1 | maze_df$y == height] <- "black"
  return(maze_df)
}
