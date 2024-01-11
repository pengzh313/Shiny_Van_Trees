# Vancouver street trees Shiny App

# Load necessary libraries for the app
library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)

# read dataset and map file
# The GeoJSON map file provided by UBC MDS (Master of Data Science) program
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
      # Sidebar with a selection input for genus 
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "genus",
                         label = "Select the genus",
                         choices = NULL
          ),
          
          br(),
          
          p("The map shows the number of trees with selected genus in each neighbourhood in the City of Vancouver")
        ),
               
        # Show a map of the distribution of the selected genus 
        mainPanel(
          leafletOutput("treemap_genus")
        )
     )
    ),
    
    tabPanel("Search by Height",
      # Sidebar with a selection input for height range 
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "height",
                         label = "Select the height range in ft",
                         choices = NULL
          ),
                 
          br(),
                 
          p("The map shows the number of trees with selected height range in each neighbourhood in the City of Vancouver")
        ),
               
        # Show a map of the distribution of the selected height range 
        mainPanel(
          leafletOutput("treemap_height")
        )
     )             
    ),
    
    tabPanel("Search by Diameter",
      # Sidebar with a selection input for diameter range 
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "diameter",
                      label = "Select the diameter range in inches",
                      value = c(0, 305),
                      min = 0,
                      max = 305
          ),
                 
          br(),
                 
          p("The map shows the number of trees with selected diameter range in each neighbourhood in the City of Vancouver")
        ),
               
      # Show a map of the distribution of the selected diameter range 
        mainPanel(
          leafletOutput("treemap_diameter")
        )
     )
    )
  ),
  
  br(),
  
  HTML("<p><center>Data retrieved in January 2024 from <a href='https://opendata.vancouver.ca/explore/dataset/street-trees/information/?disjunctive.species_name&disjunctive.common_name&disjunctive.on_street&disjunctive.neighbourhood_name'>Open Data Portal by City of Vancouver</a></p>")
)

# define a function to compute number of trees in neighborhoods
#' Modify GeoJSON file to add a new variable of the number of trees computed from a dataframe
#' with matched column of neighbourhood_name
#'
#' @param df data.frame
#' @param map SpatialPolygonsDataFrame
#' 
#' @return SpatialPolygonsDataFrame
#' 
#' @export
#' 
#' @examples
#' count_trees(df, van_map)
count_trees <- function(df, map) {
  df_count <- count(df, neighbourhood_name)
  map@data <- left_join(map@data, 
                        df_count, 
                        by = c("name" = "neighbourhood_name"))
  return(map)
}

# define server
server <- function(input, output, session) {
  ## below code for genus tab
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
  output$treemap_genus <- renderLeaflet(
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

  ## below code for height tab
  updateSelectizeInput(session, 
                       "height", 
                       choices = c("ALL", df$height_range),
                       selected = "ALL",
                       server = TRUE)
  
  df_selected_h <- reactive(
    if (input$height != "ALL") {
      filter(df, height_range == input$height)
    } else {
      df
    }
  )
  
  map_data_h <- reactive(count_trees(df_selected_h(), van_map))
  
  labels_h <- reactive(sprintf(
    "<strong>%s</strong><br/>%g trees</sup>",
    map_data_h()$name, map_data_h()$n
  ) |> lapply(htmltools::HTML))
  
  # below code modified from https://rstudio.github.io/leaflet/choropleths.html
  output$treemap_height <- renderLeaflet(
    leaflet(map_data_h()) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorBin("YlOrRd", domain = map_data_h()$n)(n),
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
        label = labels_h(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) |>
      addLegend(pal = colorBin("YlOrRd", domain = map_data_h()$n), 
                values = ~n, 
                opacity = 0.7, 
                title = "Number of trees", 
                position = "bottomright")
  )
  
  ## below code for diameter tab
  
  df_selected_d <- reactive(
    filter(df, between(diameter, input$diameter[1], input$diameter[2])
    )
  )
  
  map_data_d <- reactive(count_trees(df_selected_d(), van_map))
  
  labels_d <- reactive(sprintf(
    "<strong>%s</strong><br/>%g trees</sup>",
    map_data_d()$name, map_data_d()$n
  ) |> lapply(htmltools::HTML))
  
  # below code modified from https://rstudio.github.io/leaflet/choropleths.html
  output$treemap_diameter <- renderLeaflet(
    leaflet(map_data_d()) |>
      addTiles() |>
      addPolygons(
        fillColor = ~colorBin("YlOrRd", domain = map_data_d()$n)(n),
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
        label = labels_d(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) |>
      addLegend(pal = colorBin("YlOrRd", domain = map_data_d()$n), 
                values = ~n, 
                opacity = 0.7, 
                title = "Number of trees", 
                position = "bottomright")
  )
}

# Run the application 
shinyApp(ui, server)
