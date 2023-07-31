#Main file where the game will be run. 

source('setup_maze.R') #setup pckges & other settings
source('Maze_generation.R') #maze generation & Solver
source('animations_maze.R') # animations
#source('') User interactions & undo and reset
#source('') #timer
#source('') #Themes
source('Shiny_maze.R') # visualizations of the game

build_players_view(maze1,1)
what_player_can_see()