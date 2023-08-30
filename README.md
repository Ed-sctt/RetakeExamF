
# RetakeExamF package

<!-- badges: start -->
[![R-CMD-check](https://github.com/user/repo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/user/repo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of RetakeExamF is to allow users to build mazes based on DFS algorithm. They can also use a solver method to see the solution of the generated maze.

## Installation

You can install the development version of RetakeExamF from [GitHub](https://github.com/Ed-sctt/RetakeExamF) with:

``` r
# install.packages("RetakeExamF")
devtools::install_github("Ed-sctt/RetakeExamF") 

# call the package
library(RetakeExamF)
```
## Description 

RetakeExamF is a dynamic R package that empowers users to craft captivating mazes using the Depth-First Search (DFS) algorithm. Seamlessly merging educational insights and creative possibilities, this package provides a user-friendly platform to generate mazes of varying complexities.

By integrating the "igraph" package, the creation of networks composed of edges and vertices, improve the flexibility and customization of maze designs. "ggplot2" package further elevates the maze-building experience by enabling users to visualize their mazes with aesthetically pleasing layouts.


## Steps to follow to get a maze

### 1) Make a graph 
Define the number of rows and cols to give a dimension to the graph and assign it name.

### 2) Use DFS method
Add your name_graph in the function that create the maze with DFS.

### 3) Plot the graph
Plot your name_graph.

You can have a glimpse of the final output that the package offers :


## Example

This is a basic example which shows you how to solve a common problem:

```r
Example_maze <- RetakeExamF::myGraph (nrows = 10,ncols = 10) 
Example_maze <- RetakeExamF::dfs_method (GraphMade = Example_maze)
RetakeExamF::plotmyMaze(Example_maze,nrows= 10,ncols = 10)

## To plot a maze of size 10x10
```
It is also possible to display the solution path of the existing maze as below :

```r
RetakeExamF::solverGraph(Example_maze,nrows= 10,ncols = 10)
## Visualization of the solution of the exiting maze

```

Finally the package is compatible with Shiny. 
```r
RetakeExamF::myMazeShinyApp3() 
## shiny app experience 

```

## Additional resources - (annexe)

You can find more information on DFS algorithm to build a maze here : 

https://www.algosome.com/articles/maze-generation-depth-first.html <br> 
https://www.geeksforgeeks.org/random-acyclic-maze-generator-with-given-entry-and-exit-point/
https://github.com/Vessy/Rmaze/tree/master
https://github.com/ehermo/monster-maze-shiny-mobile/blob/master/game/monster-maze-all.R



Please, to know more about the different functions included in igraph and ggplot2 packages, refer to the resources below : 

https://r.igraph.org/ </br> 
https://ggplot2.tidyverse.org/



