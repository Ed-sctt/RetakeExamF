# packages 
pckg <- install.packages(c('devtools','shiny','purr'), quiet = T)
purrr::walk(pckg, library, character.only = TRUE)

library(devtools)
devtools::install_git('https://github.com/Vessy/Rmaze.git') # resource package
library(Rmaze)
library(shiny)

#generate maze 

#a <- Rmaze::makeGraph(20,10)
#a<-makeMaze_kruskal(a) # krustal aglgo can be developped 
#plotMaze(a,20,10)
#Rmaze::plotMazeSolution(a,20,10)
# developpemt of the different maze algo ??? 
##  kruskal 
##  dfs
## prim 

# shiny part 

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Building mazes in R"),
  
  fluidRow(
    column(4,
           
           wellPanel(
             h4("Define maze:"),
             sliderInput("width", "Maze width", 5, 100, 10, step = 1),
             sliderInput("height", "Maze height", 5, 100, 10, step = 1),
             
             selectInput("method", "Select maze generation method",
                         c("Recursive backtracker" = "rbt",
                           "Kruskal's algorithm" = "ka",
                           "Prim's algorithm" = "pa")),
             
             checkboxInput(inputId = "imperfect",
                           label = strong("Create imperfect maze"),
                           value = FALSE),
             
             actionButton("gmButton", "Generate maze"),
             actionButton('Solver','Solve maze')
           ))),
  fluidRow(
    column(6,
           mainPanel(
             wellPanel(
               h4("Maze:"),
               plotOutput("plotMaze")
             ))),
    column(6,
           mainPanel(
             wellPanel(
               h4('Solver:'),
               plotOutput(outputId = 'solveMaze')
             )
           ))))
)


server <- shinyServer(function(input, output) {
  
  maze <- eventReactive(input$gmButton, {
    
    width <- input$width
    height <- input$height
    method <- input$method
    
    g <- makeGraph(width, height)
    
    myMaze <- switch(method,
                     "rbt" = makeMaze_dfs(g, inShiny=TRUE),
                     "ka" = makeMaze_kruskal(g, inShiny=TRUE),
                     "pa" = makeMaze_prim(g, inShiny=TRUE))
    
    if (input$imperfect) {
      myMaze <- makeImperfect(myMaze, inShiny=TRUE)
    }
    
    myMaze
    
  })
  
  output$plotMaze <- renderPlot({
    width <- isolate(input$width)
    height <- isolate(input$height)
    plotMaze(maze(), width, height, inShiny=TRUE)
  })
  
  ### solver function
  solver <- eventReactive(input$Solver, {
  maze() # recall maze function
   })
  
  output$solveMaze <- renderPlot({
    width <- isolate(input$width)
    height <- isolate(input$height)
    plotMazeSolution(solver(), width, height,inShiny = TRUE)
  })
  
})

myapp <- shinyApp(ui,server)
myapp
