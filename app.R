# Vancouver street trees Shiny App

# Load necessary libraries for the app
library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)

# read dataset and map file
df <- read_delim("data/street_trees_cleaned.csv")
url_geojson = "https://raw.githubusercontent.com/UBC-MDS/exploratory-data-viz/main/data/local-area-boundary.geojson"
van_map <- geojsonio::geojson_read(url_geojson, what = "sp")

# define UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  # Application title
  titlePanel(
    h1(strong("Vancouver Street Trees"), align = "center")
  ),
  
  tabsetPanel(
    tabPanel("Search by Genus",
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "genus",
                         label = "Select the genus",
                         choices = NULL
          ),
          
          br(),
          
          p("The map shows the number of selected trees in each neighbourhood in the City of Vancouver")
        ),
               
        # Show a map of the distribution of the selected genus 
        mainPanel(
          leafletOutput("treemap")
        )
     )
    ),
    
    tabPanel("Search by Hight",
    )
  ),
  
  br(),
  
  HTML("<p><center>Data retrieved in January 2024 from <a href='https://opendata.vancouver.ca/explore/dataset/street-trees/information/?disjunctive.species_name&disjunctive.common_name&disjunctive.on_street&disjunctive.neighbourhood_name'>Open Data Portal by City of Vancouver</a></p>")
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
                       choices = c("ALL", df$genus_name),
                       selected = "ALL",
                       server = TRUE)
  
  df_selected <- reactive(
    if (input$genus != "ALL") {
      filter(df, genus_name == input$genus)
    } else {
      df
    }
  )

  map_data <- reactive(count_trees(df_selected(), van_map))
  
  labels <- reactive(sprintf(
    "<strong>%s</strong><br/>%g trees</sup>",
    map_data()$name, map_data()$n
  ) |> lapply(htmltools::HTML))
  
  # below code modified from https://rstudio.github.io/leaflet/choropleths.html
  output$treemap <- renderLeaflet(
    leaflet(map_data()) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorBin("YlOrRd", domain = map_data()$n)(n),
        weight = 2,
        opacity = 1,
        color = "blue",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) |>
      addLegend(pal = colorBin("YlOrRd", domain = map_data()$n), 
                values = ~n, 
                opacity = 0.7, 
                title = "Number of trees", 
                position = "bottomright")
  )
}

# Run the application 
shinyApp(ui, server)
