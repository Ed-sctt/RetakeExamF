#'@title Building mazes app 
#' @author Joost and Edward
#' @details 
#' According to shiny settings, the app offers the user to build a maze by selecting the inputs
#' weight, height, and the Depth First Search algorithm. 

#' @return The function returns a shiny app with the possibility to build a maze
#' and see its solution. 
#' @examples
#' \dontrun{myMazeShinyApp()}

#' @export
myMazeShinyApp <- function(){
  
  #user interface part
  
  ui <- shiny::shinyUI(shiny::fluidPage(
  
  # Application title
    shiny::titlePanel("Building mazes in R"),
  
    shiny::fluidRow(
      shiny::column(4,
                    shiny::wellPanel(
                      shiny::h4("Define maze:"),
                      shiny::sliderInput("width", "Maze width", 5, 100, 10, step = 1),
                      shiny::sliderInput("height", "Maze height", 5, 100, 10, step = 1),
             
                      shiny::selectInput("method", "Select maze generation method",
                         c("Recursive backtracker" = "rbt",
                           "Kruskal's algorithm" = "ka",
                           "Prim's algorithm" = "pa")),
             
                      shiny::checkboxInput(inputId = "imperfect",
                           label = shiny::strong("Create imperfect maze"),
                           value = FALSE),
             
                      shiny::actionButton("gmButton", "Generate maze"),
                      shiny::actionButton('Solver','Solve maze')
           ))),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::mainPanel(
                      shiny::wellPanel(
                        shiny::h4("Maze:"),
                        shiny::plotOutput("plotMaze")
             ))),
      shiny::column(6,
                    shiny:: mainPanel(
                      shiny::wellPanel(
                        shiny::h4('Solver:'),
                        shiny::plotOutput(outputId = 'solveMaze')
             )
           ))))
  )

  # server part 
  
  server <- shiny::shinyServer(function(input, output) {
    
    maze <- shiny::eventReactive(input$gmButton, {
    
    width <- input$width
    height <- input$height
    method <- input$method
    
    g <- RetakeExamF::myGraph(nrows = width, ncols = height) #old makeGraph()
    
    myMaze <- switch(method,
                     "rbt" = RetakeExamF::dfs_method(g))#old (g, inShiny =T)
                     #"ka" = makeMaze_kruskal(g, inShiny=TRUE),
                     #"pa" = makeMaze_prim(g, inShiny=TRUE))
    
    #if (input$imperfect) {
     # myMaze <- makeImperfect(myMaze, inShiny=TRUE)
    #}
    
    #myMaze
    
  })
  
  output$plotMaze <- shiny::renderPlot({
    width <- shiny::isolate(input$width)
    height <- shiny::isolate(input$height)
    RetakeExamF::plotmyMaze(maze(), width, height)
  })
  
  ### solver function
  solver <- shiny::eventReactive(input$Solver, {
  maze() # recall maze function
   })
  
  output$solveMaze <- shiny::renderPlot({
    width <- shiny::isolate(input$width)
    height <- shiny::isolate(input$height)
    RetakeExamF::solverGraph(solver(),nrows = width, ncols = height)
  })
  })
  
  #run the app
  
  myapp <- shiny::shinyApp(ui,server)
  return(myapp)
}
