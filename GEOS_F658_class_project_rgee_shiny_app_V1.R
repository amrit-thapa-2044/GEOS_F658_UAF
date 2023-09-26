
rm(list=ls())

library(rgee)

ee_Initialize('athapa2@alaska.edu')

# Define UI
ui <- fluidPage(
  titlePanel("Data Extraction Tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Enter Location (e.g., lon, lat):"),
      dateRangeInput("date_range", "Select Date Range:"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      plotOutput("ndsi_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$submit_button, {
    # Extract user input
    location <- as.character(input$location)
    date_range <- input$date_range
    print(date_range);print(location)
    
    # Convert date range to EE date format
    start_date <- format(date_range[1], "%Y-%m-%d")
    end_date <- format(date_range[2], "%Y-%m-%d")
    
    # Create a geometry from the user-supplied location
    point <- ee$Geometry$Point(as.numeric(strsplit(location, ",")[[1]]))
    
    S1_collection <- ee$ImageCollection('COPERNICUS/S1_GRD')$
      filterBounds(point)$
      filter(ee$Filter$eq('instrumentMode', 'IW'))$
      filter(ee$Filter$eq('orbitProperties_pass', 'ASCENDING'))$
      filterDate(start_date, end_date)$
      select('VV')
    
    # print(S1_collection)
    
    
    df_point <- ee_extract(x = S1_collection, y =  point, fun = ee$Reducer$mean(), scale = 20,sf=FALSE)
    # print(head(df_point))
    
    df_plot=data.frame(Date=NA,Data=as.numeric(df_point[1,]))
    
    
     # Render the plot
    output$ndsi_plot <- renderPlot({
      plot(df_plot$Data,type='l')
    }) # end of plot
  }) # end of input
} # end of server

# Run the app
shinyApp(ui = ui, server = server)
