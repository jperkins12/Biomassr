library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Forest Biomass Estimator"),
  
  sidebarLayout(
    sidebarPanel(
                 fluidRow(
                   
                   # input csv file with tree diameter data
                   column(12, fileInput("datafile", "File input",
                             accept=c('text/csv', 'text/comma-separated-values,text/plain')))
                 ),
                 
                 # selector for id column
                 fluidRow(column(12, uiOutput("idcol"))),
                 
                 # selector for species column
                 fluidRow(column(12, uiOutput("speciescol"))),
                 
                 # selector for dbh column
                 fluidRow(column(12, uiOutput("dbhcol"))),
                 
                 # choose units for dbh
                 fluidRow(column(12, radioButtons("units", "DBH Units",
                                       choices = list("Cm" = 1, "Inches" = 2),selected = 1))),
                 
                 # The action button prevents an action firing before we're ready
                 fluidRow(
                   column(6, actionButton("getbiomass", "Calculate Biomass")),
                 
                   # download button
                   column(6, fluidRow(uiOutput("dloader"))))
    ),
    mainPanel(
      tabsetPanel(id = "maintab",
                  
        tabPanel("Introduction",
                 h2("Welcome to Biomassr"),
                 p("Biomassr is a utility that generates above ground biomass estimations[1] for forest observations."),
                 h4("Instructions:"),
                 tags$ol(
                   tags$li("Upload a .csv file that contains at least three fields."),
                   tags$ol(
                     tags$li("A unique ID field."),
                     tags$li("A speices field."),
                     tags$li("A DBH field.")
                   ),
                   tags$li("Use the drop-downs on the left to select the appropriate fields."),
                   tags$li("Verify the proper units for DBH in the menu to the left."),
                   tags$li("In the \"Species\" tab, select the appropriate species group"),
                   tags$li("Click, \"Calculate Biomass\"")
                 ),
                 h4("References:"),
                 p("[1] Jenkins, J. et al., 2003. National scale biomass estimates for United States tree species. Forest Science, 49(1), pp.12–32.")
                 ),
        # display for data preview
        tabPanel('Raw Data',
                 uiOutput("nodata"),
                 tableOutput("filetable")),
        
        tabPanel("Species", value = "speciestab",
                 uiOutput("specieslist")),
        
        #display biomass results
        tabPanel("Results", value = "resultstab",
                 uiOutput("noresults"),
                 tableOutput("biomasstable"))
        )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # display text if no table loaded
  output$nodata <- renderUI({
    if (is.null(filedata())){
      "No data to display"
    }
  })
  
  output$noresults <- renderUI({
    if (is.null(biocalc())) {
      "No results loaded"
    }
  })
  
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
    filedata()
  })
  
  output$biomasstable <- renderTable({
    biocalc()
  })
  
  #The following set of functions populate the column selectors
  output$idcol <- renderUI({
    df <-filedata()
    req(df)
    
    items=names(df)
    names(items) = items
    selectInput("id", "ID Column",items)
  })
  
  output$speciescol <- renderUI({
    df <-filedata()
    req(df)
    
    items=names(df)
    names(items) = items
    selectInput("species", "Species Column",items)
  })
  
  output$dbhcol <- renderUI({
    df <-filedata()
    req(df)
    
    items=names(df)
    names(items) = items
    selectInput("dbh", "DBH Column",items)
  })
  
  output$specieslist <- renderUI({
    df <-filedata()
    req(df)
    speciescol = input$species
    
    # get list of plot species
    specieslist = as.character(unique(df[,c(speciescol)]))
    
    # get list of jenkins species groups
    grouppath = file.path("src", "csv", "bparams.csv")
    grouptable = read.csv(grouppath)
    groups = sort(grouptable$species.group)
    
    # create selectinput for each species in plot
    lapply(specieslist, function(i) {fluidRow(column(12, selectInput(i, label = i, choices = groups)))})
  })
  
  # jump to results tab on 
  observeEvent(input$getbiomass, {
    updateTabsetPanel(session, "maintab",
                      selected = "resultstab")
  })
  
  # calculate biomass when calculate button is hit
  biocalc <- reactive({
    if (input$getbiomass == 0) return(NULL)
    req(filedata())
    
    #The function acts reactively when one of the variables it uses is changed
    #If we don't want to trigger when particular variables change, we need to isolate them 
    isolate({
      #Get the CSV file data
      treedata = filedata()
      #Which from/to columns did the user select?
      id = input$id
      species = input$species
      dbh = input$dbh
      
      # check that chosen fields are all different
      if (!anyDuplicated(c(id, species, dbh)) == 0) {
        showNotification("ERROR: Columns need to be unique!", duration = NULL, type = "error")
        return(NULL)
      }
      
      # convert inches to cm if selected
      if (input$units == 2) {
        treedata[,c(dbh)] = treedata[,c(dbh)] * 2.54
      }
      
      # get selected columns
      treedata = treedata[,c(id,species,dbh)]
      
      # convert species to jenkins specific types
      treedata[,c(species)] = sapply(as.character(treedata[,c(species)]), function(i) {input[[i]]})
      
      # run the calculation function
      source(file.path('core', 'R', 'biomass_functions.R'))
      biomassData = plotMass(treedata, species, dbh)
      
      # check for NA values
      if (anyNA(biomassData$biomass)) {
        showNotification("One or more rows contains invalid values!", duration = NULL, type = "error")
      }
      
      # rename columns and print output
      colnames(biomassData) <- c('Tree ID', 'Species Group', 'DBH (cm)', 'Biomass (kg)')
      return(biomassData)
    })
    
  })
  
  # show download button when results are present
  output$dloader <- renderUI({
    
    req(biocalc())
    downloadButton("downloadData", "Download")
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = "forest_biomass.csv",
    content = function(file) {
      write.csv(biocalc(), file)
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)