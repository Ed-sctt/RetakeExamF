## Shiny app

# code needs to be re-written , this is only copy/pasting , no value added ! 

# User interface

ui <- fluidPage(
  useShinyjs(),
  #titlePanel("Lundi, début de semaine..."),
  fluidRow(
    column(12,
           # https://groups.google.com/g/shiny-discuss/c/8GmXV-UfTm4?pli=1
           #verbatimTextOutput("banner"),
           #align="center",
           tags$head(tags$style(HTML("
                            #banner {
                              font-size: 4px;
                              background-color: black;
                              color:green;
                            }
                            "))),
           style ="font-size:10px;")),
  fluidRow(
    column(12,
           # "Level & Lives",
           verbatimTextOutput("level"),
           tags$head(tags$style(HTML("
              #level {
                font-size: 15px;
                background-color: black;
                color:green;
               }"))),
           style ="font-size:20px;")),
  
  fluidRow(
    column (12,
            # "Player's View",
            htmlOutput("players_view"),
            align="center",
            tags$head(tags$style(HTML("
            #players_view {
              font-size: 15px;
              background-color: white;
              color:black;
            }"))),
            style ="font-size:20px;")),
  fluidRow(
    column(12,
           actionButton("left","L",  icon = icon("arrow-left", lib="glyphicon"), width = "30%", class = "btn-success", style="padding: 10px 20px; border-radius: 10px;"), 
           actionButton("forward","F",  icon = icon("arrow-up", lib="glyphicon"),width = "30%", class = "btn-info",  style="padding: 10px 20px; border-radius: 10px;"),
           actionButton("right","R", icon = icon("arrow-right", lib="glyphicon"),width = "30%", class = "btn-warning", style="padding: 10px 20px; border-radius: 10px;"), align="center"
    ),style ="padding-top:0.5em;margin-bottom:0.5em;"),
  fluidRow(
    column(12,
           #"Console",
           verbatimTextOutput("console"),
           tags$head(tags$style(HTML("
                            #cosole {
                              font-size: 15px;
                            }"))),
           style ="font-size:20px;padding-top:0.5em;"
    )),
  fluidRow(
    column(12,
           "Buttons",
           verbatimTextOutput("keys"),
           tags$head(tags$style(HTML("
              #keys {
                font-size: 15px;
                background-color: black;
                color:green;
              }"))),
           style ="font-size:20px;")
  ),
  style="background-color: black;color:green;"
)



## server part 
server <- function(input,output,session){}





# run the app
maze_app<-shinyApp(ui,server)
runApp(maze_app)
