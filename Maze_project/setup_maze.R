## Maze project

## resources
# https://www.r-bloggers.com/2023/01/shiny-monster-maze/ #Nomming and syntax
# https://github.com/ehermo/monster-maze-shiny-mobile/blob/master/game/monster-maze-all.R

## packages

#revoir la logique entière du code 
pckg <- install.packages(c('collections','knitr', 'invctr', 'beepr', 'dplyr', 'stringr','purrr','Dict','shiny',
                           'howler','shinyjs'),
          quiet = TRUE)
purrr::walk(pckg, library, character.only = TRUE)




# Constants
#NONE <- -1
WALL <- 0
CORRIDOR <- 1
EXIT <- 9
#GHOST <- 2
#ZOMBIE <- 3
PLAYER <- 5
DIRECTIONS <- c("N", "E", "S", "W")


# # Action map
# * key (name):""
# * value (action): list
# * * desc:""
# * * keys (keyboard):c()
# * * echo:""


TURNL<- list("desc" = "turn left",
              "keys" = c('left arrow'),
              "echo" = "%s: turning left")
class(TURNL)
WALK <- list("desc" = "walk forward ↑F",
             "keys" = c("up arrow"),
             "echo" = "%s: moving forward ↑")
class(WALK)

TURNR <- list("desc" = "turn right →R", 
              "keys" = c("right arrow"),
              "echo" = "%s: turning right →")
class(TURNR)

convert_to_invctr_position <- function(position) {
  invctr_position <- data.frame(nv=1, row=position$row, col=position$col, row.names = NULL)
}

# define position in the maze
new_position <- function (row,col) {
  return (list("row" = as.integer(row),
               "col" = as.integer(col)))
}

#
get_random_direction <- function() {
  sample(DIRECTIONS,1)
}

#
can_move_to <- function(maze, destination) {
  return (maze[destination$row,destination$col] == CORRIDOR ||
            maze[destination$row,destination$col] == EXIT)
}

#
is_exit <- function(maze,position) {
  return (maze[position$row,position$col] == EXIT)
}

#
get_position_forward <- function(current_position, direction) {
  position_forward <- current_position
  if(direction == "N") {
    position_forward$row <- position_forward$row - 1
  } 
  else if (direction == "S") {
    position_forward$row <- position_forward$row + 1
  }
  else if (direction == "W") {
    position_forward$col <- position_forward$col - 1
  }
  else if (direction == "E") {
    position_forward$col <- position_forward$col + 1 
  }
  position_forward
}

#
calc_distance <- function(position_1, position_2) {
  row_distance <- abs(position_1$row - position_2$row)
  col_distance <- abs(position_1$col - position_2$col)
  distance <- row_distance + col_distance
}

# 
is_next_to <- function(position_1, position_2, max_distance) {
  distance <- calc_distance(position_1 = position_1, position_2 = position_2)
  if(distance > max_distance) {
    return(FALSE)
  }
  TRUE
}

#
is_player_next_to_any_ghost <-  function(player_position, positions) {
  if(length(positions) == 0) {
    return (FALSE)
  }
  Reduce('|',lapply(positions,is_next_to,position_2 = player_position, max_distance = 1))
}

##
is_player_caught_by_any_zombie <- function(player_position, zombie_positions) {
  if(length(zombie_positions) == 0) {
    return (FALSE)
  }
  Reduce('|',lapply(zombie_positions,is_next_to,position_2 = player_position, max_distance = 0))
}

what_player_can_see <- function (maze, 
                                 player_position,
                                 direction, 
                                 ghost_positions,
                                 zombie_positions, 
                                 forward_vision = 4, 
                                 rear_vision = 0) {
  lateral_vision <- floor((forward_vision - 1)/2)
  padding <- forward_vision
  number_rot <- 0
  meta_maze <- matrix(0,nrow(maze) + (2 * padding), ncol(maze) + (2 * padding))
  meta_maze[(1 + padding):(nrow(maze) + padding ), (1 + padding):(ncol(maze) + padding)] <- maze
  for (ghost_position in ghost_positions) {
    meta_maze[ghost_position$row + padding,ghost_position$col + padding] <- GHOST
  }
  for (zombie_position in zombie_positions) {
    meta_maze[zombie_position$row + padding,zombie_position$col + padding] <- ZOMBIE
  }
  if(direction == "N") {
    start_row <- player_position$row  - forward_vision
    end_row <- player_position$row + rear_vision
    start_col <- player_position$col - lateral_vision
    end_col <- player_position$col + lateral_vision 
    number_rot <- 0
  }
  else if (direction == "W") {
    start_row <- player_position$row - lateral_vision
    end_row <- player_position$row + lateral_vision 
    start_col <- player_position$col - forward_vision
    end_col <- player_position$col + rear_vision
    number_rot <- 1
  }
  else if (direction == "S") {
    start_row <- player_position$row - rear_vision
    end_row <- player_position$row + forward_vision
    start_col <- player_position$col - lateral_vision
    end_col <- player_position$col + lateral_vision 
    number_rot <- 2
  }
  else if (direction == "E") {
    start_row <- player_position$row - lateral_vision
    end_row <- player_position$row + lateral_vision
    start_col <- player_position$col - rear_vision
    end_col <- player_position$col + forward_vision
    number_rot <- 3
  }
  start_row <- start_row + padding
  end_row <- end_row + padding
  start_col <- start_col + padding
  end_col <- end_col + padding
  maze_view <- meta_maze[start_row:end_row, start_col:end_col]
  
  count_rot <- 0
  while(count_rot < number_rot)
  {
    maze_view <- rotate_clockwise(maze_view)
    count_rot <- count_rot + 1
  }
  
  maze_view[nrow(maze_view) - rear_vision,lateral_vision  + 1] <- PLAYER
  maze_view
}


#
get_graphics <- function(maze_view,graph_map) {
  nrow <- nrow(maze_view)
  ncol <- ncol(maze_view)
  matrix(lapply(lapply(c(maze_view),graph_map$get),function(x) {return (paste0("<td style='padding-top:3px;margin-top:3px;'>",x$block,"</td>",sep=""))}), nrow,ncol)
}


build_players_view <- function(maze,
                               forward_vision, 
                               rear_vision){
  
  maze_view <- what_player_can_see(maze = maze,
                                   player_position = player_position, 
                                   direction = player_direction,
                                   forward_vision = forward_vision,
                                   rear_vision  = rear_vision)
  
  view = get_graphics(maze_view,graph_map)
  map_height <- nrow(view)
  pane2_height <- map_height + 3
  pane2 <- matrix("", nrow = pane2_height, ncol = 1 )
  colnames(pane2) <- c("Map")
  map_idx <- 1
  pane2[map_idx,"Map"] <- '<table style="border-spacing:0;border-collapse:collapse;line-height:1.1em;background-color:white;color:black" border="0" borderspacing="0"'
  map_idx <- map_idx + 1
  for (line in apply(view, 1, paste, collapse = "")) {
    pane2[map_idx,"Map"] <- paste0('<tr style="padding:0;margin:0px;">',line,'</tr>')
    map_idx <- map_idx + 1
  }
  pane2[map_idx,"Map"] <- '</table'
  return(paste(pane2,collapse=""))
}

#player_position,
#ghost_positions,
#zombie_positions, 
#player_direction, 
#ghost_positions = ghost_positions, 
#zombie_positions = zombie_positions,





