#'@title Building mazes app 
#' @author Joost and Edward
#' @details 
#' According to shiny settings, the app offers the user to build a maze by selecting the inputs
#' weight, height, and the Depth First Search algorithm. 

#' @return The function returns a shiny app with the possibility to build a maze
#' and see its solution. 
#' @examples
#' \dontrun{myMazeShinyApp()}
#' 
# define global functions 
utils::globalVariables(c('observeEvent','renderImage'))

#' @export
myMazeShinyApp <- function(){
  
  #user interface part
  
  ui <- shiny::shinyUI(
    shiny::navbarPage('Maze App',
                      position = 'static-top',
                      theme = shinythemes::shinytheme('yeti'),
                      
                      shiny::tabPanel("Build Maze",
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
                                                          shiny::plotOutput(outputId = 'solveMaze')
                                                        )
                                                      )))
                      )
    )
  )
  
  
  # server part 
  
  server <- shiny::shinyServer(function(input, output) {
    
    maze <- shiny::eventReactive(input$Button, {
      
      width <- input$width
      height <- input$height
      
      
      g <- RetakeExamF::myGraph(nrows = width, ncols = height) #graph
      
      myMaze <-  RetakeExamF::dfs_method(g) # maze
      
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
    
    # display an image on the mainPanel
    observeEvent(input$Button, {
      filename <- "img/text (1).gif"  
      output$myImage <- renderImage({
        list(src = filename,
             contentType = 'image/gif',
             width = 862,
             height = 100,
             alt = "Test")
      }, deleteFile = FALSE)
    })
  })
  
  #run the app
  
  myapp <- shiny::shinyApp(ui,server)
  return(myapp)
}
myMazeShinyApp()

