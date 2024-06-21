app <- shinyApp(
  ui = fillPage(
    fillCol(flex = c(NA, 1), 
            inputPanel(
              selectInput("route", "Route:", choices = routes_sf)
            ),
            leafletOutput("map")
    )
  ),
  server = function(input, output) {
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -80.8431, lat = 35.2271, zoom = 12) %>%
        addPolylines(data = routes_sf[,input$route], color = "red", stroke = 0.5, opacity = 0.5)
    })
  }
)