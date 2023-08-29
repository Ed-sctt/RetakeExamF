#' @title Building mazes app 
#' @author Joost and Edward
#' @details 
#' According to shiny settings, the app offers the user to build a maze by selecting the inputs
#' weight, height. Two algorithms are used : DFS and Recursive Backtracking  

#' @return The function returns a shiny app with the possibility to build a maze
#' and see its solution. 
#' @examples
#' \dontrun{myMazeShinyApp3()}
#' @name myMazeShinyApp3 
#'
# define global functions 
utils::globalVariables(c('observeEvent','renderImage'))

#' @export

myMazeShinyApp3 <- function(){
  
  # User interface part
  ui <- shiny::shinyUI(
    shiny::navbarPage('Maze App',
                      position = 'static-top',
                      theme = shinythemes::shinytheme('yeti'),
                      shiny::tabsetPanel(
                        shiny::tabPanel("Tab 1",
                                        shiny::titlePanel("Maze actions"),
                                        
                                        shiny::fluidRow(
                                          shiny::column(4,
                                                        shiny::wellPanel(
                                                          shiny::h4("Define maze options:"),
                                                          shiny::sliderInput("width", "Maze width", 5, 100, 10, step = 1),
                                                          shiny::sliderInput("height", "Maze height", 5, 100, 10, step = 1),
                                                          shiny::actionButton("Button", "Generate maze"),
                                                          shiny::actionButton('Solver','Solve maze')
                                                        ))),
                                        # Display the image
                                        shiny::mainPanel(
                                          shiny::imageOutput("myImage", width = 862, height = 100)
                                        ),
                                        
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
                                                            shiny::plotOutput(outputId = 'solveMaze')))))
                                        ),
                        shiny::tabPanel('Tab 2',
                                        shiny::h1('Recursive backtracking maze'),
                                        shiny::plotOutput('rbmaze')))
    )
  )
  
  # Server part 
  server <- shiny::shinyServer(function(input, output) {
    
    # Maze generation
    maze <- shiny::eventReactive(input$Button, {
      width <- input$width
      height <- input$height
      
      g <- RetakeExamF::myGraph(nrows = width, ncols = height) # graph
      myMaze <-  RetakeExamF::dfs_method(g) # maze
      
      return(myMaze)
    })
    
    # Maze plot
    output$plotMaze <- shiny::renderPlot({
      width <- shiny::isolate(input$width)
      height <- shiny::isolate(input$height)
      RetakeExamF::plotmyMaze(maze(), width, height)
    })
    
    # Solver function
    solver <- shiny::eventReactive(input$Solver, {
      maze() # recall maze function
    })
    
    # Solver output plot
    output$solveMaze <- shiny::renderPlot({
      width <- shiny::isolate(input$width)
      height <- shiny::isolate(input$height)
      RetakeExamF::solverGraph(solver(), nrows = width, ncols = height)
    })
    
    # Joost maze plot
    maze3 <- shiny::eventReactive(input$Button, {
      width <- shiny::isolate(input$width)
      height <- shiny::isolate(input$height)
      
      rb_maze(width, height)
    })
    
    output$rbmaze <- shiny::renderPlot({
      maze3()
    })
    
    # display an image on the mainPanel
    observeEvent(input$Button, {
      filename <- "inst/img/text.gif"  
      output$myImage <- renderImage({
        list(src = filename,
             contentType = 'image/gif',
             width = 862,
             height = 100,
             alt = "Test")
      }, deleteFile = FALSE)
    })
    
  })
  
  # Run the app
  myapp <- shiny::shinyApp(ui, server)
  return(myapp)
}
