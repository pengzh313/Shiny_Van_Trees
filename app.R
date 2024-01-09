# Vancouver street trees Shiny App

# Load necessary libraries for the app
library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)

# read dataset and map
df <- read_delim("data/street_trees_cleaned.csv")
url_geojson = "https://raw.githubusercontent.com/UBC-MDS/exploratory-data-viz/main/data/local-area-boundary.geojson"
van_map <- geojsonio::geojson_read(url_geojson, what = "sp")

# define UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  # Application title
  titlePanel("Vancouver Street Trees"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "genus",
                     label = "Select the genus",
                     choices = NULL
      )
    ),
    
    # Show a map of the distribution of the selected genus 
    mainPanel(
      leafletOutput("treemap")
    )
  )
)

# define a function to compute number of trees in neighborhoods
count_trees <- function(df, map) {
  df_count <- count(df, neighbourhood_name)
  map@data <- left_join(map@data, 
                        df_count, 
                        by = c("name" = "neighbourhood_name"))
  return(map)
}

# define server
server <- function(input, output, session) {
  updateSelectizeInput(session, 
                       "genus", 
                       choices = df$genus_name,
                       server = TRUE)
  
  df_selected <- reactive(
    filter(df, genus_name == input$genus))
  
  map_data <- reactive(count_trees(df_selected(), van_map))
  
  output$treemap <- renderLeaflet(
    leaflet(map_data()) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorBin("YlOrRd", domain = map_data()$n)(n),
        weight = 2,
        opacity = 1,
        color = "blue",
        dashArray = "3",
        fillOpacity = 0.7) |>
      addLegend(pal = colorBin("YlOrRd", domain = map_data()$n), 
                values = ~n, 
                opacity = 0.7, 
                title = "Number of trees", 
                position = "bottomright")
  )
}

# Run the application 
shinyApp(ui, server)
