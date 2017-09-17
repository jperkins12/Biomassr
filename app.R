library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Forest Biomass Estimator"),
  
  sidebarLayout(
    sidebarPanel(h2("Load Data Table"),
                 fluidRow(
                   
                   # input csv file with tree diameter data
                   fileInput("datafile", h3("File input"),
                             accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                 ),
                 
                 # selector for species column
                 fluidRow(uiOutput("idcol")),
                 
                 # selector for species column
                 fluidRow(uiOutput("speciescol")),
                 
                 # selector for species column
                 fluidRow(uiOutput("dbhcol")),
                 
                 # choose units for dbh
                 fluidRow(radioButtons("radio", h3("DBH Units"),
                                       choices = list("Cm" = 1, "Inches" = 2),selected = 1)),
                 
                 #The action button prevents an action firing before we're ready
                 fluidRow(actionButton("getbiomass", "Calculate Biomass"))
    ),
    mainPanel(h2("Data Display"),
              
              # display for data preview
              tableOutput("filetable"),
    
              # results output
              tableOutput("biomasstable"))
  )
)

# Define server logic ----
server <- function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    bio = biocalc()
    if (is.null(bio)) {
      filedata()
    } else {
      biocalc()
    }
  })
  
  #The following set of functions populate the column selectors
  output$idcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items) = items
    selectInput("id", "ID Column",items)
  })
  
  output$speciescol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items) = items
    selectInput("species", "Species Column",items)
  })
  
  output$dbhcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items) = items
    selectInput("dbh", "DBH Column",items)
  })
  
  # calculate biomass when calculate button is hit
  biocalc <- reactive({
    if (input$getbiomass == 0) return(NULL)
    df = filedata()
    if (is.null(df)) return(NULL)
    
    #The function acts reactively when one of the variables it uses is changed
    #If we don't want to trigger when particular variables change, we need to isolate them 
    isolate({
      #Get the CSV file data
      treedata = filedata()
      #Which from/to columns did the user select?
      id = input$id
      species = input$species
      dbh = input$dbh
      #run the calculation function
      treedata = treedata[,c(id,species,dbh)]
      source(file.path('core', 'R', 'biomass_functions.R'))
      biomassData = plotMass(treedata, species, dbh)
      colnames(biomassData) <- c('Tree ID', 'Species Group', 'DBH (cm)', 'Biomass (kg)')
      return(biomassData)
    })
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)