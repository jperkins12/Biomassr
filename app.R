library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Forest Biomass Estimator"),
  
  sidebarLayout(
    sidebarPanel(h2("Load Data Table"),
                 fluidRow(
                   fileInput("file", h3("File input"))
                   ),
                 fluidRow(selectInput("select", h3("Species Column"), 
                                      choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                     "Choice 3" = 3), selected = 1)
                          ),
                 fluidRow(selectInput("select", h3("DBH Column"), 
                                      choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                     "Choice 3" = 3), selected = 1)
                          ),
                 fluidRow(radioButtons("radio", h3("DBH Units"),
                                       choices = list("Cm" = 1, "Inches" = 2),selected = 1))
                 ),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)