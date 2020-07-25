#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      6,
      selectInput(
        ns("mapTexture"),
        "Map Texture",
        choices = list(
          "OpenTopoMap" = "OpenTopoMap",
          "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
          "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
          "Stamen.Toner" = "Stamen.Toner",
          "CartoDB.Positron" = "CartoDB.Positron",
          "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
          "Stamen.Watercolor" = "Stamen.Watercolor",
          "Stamen.Terrain" = "Stamen.Terrain",
          "Esri.WorldImagery" = "Esri.WorldImagery",
          "Esri.WorldTerrain" = "Esri.WorldTerrain"
        ),
        selected = "OpenTopoMap"
      )
    ),
    column(
      6,
      selectInput(
        ns("mapColor"),
        "Points Color",
        choices = list(
          "Red" = 'red',
          "Green" = "green",
          "Blue" = "blue",
          "Black" = "black"
        ),
        selected = "blue"
      )
    ),
    leafletOutput(ns("mymap"), height = "400")
  )
}
    
#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session, data_reactive, data_original){
  ns <- session$ns
  
  output$mymap <- renderLeaflet({
    print("leaflet")
    dat <- data_reactive$data
    validate(
      need(length(dat)>0, 'Please upload/download a dataset first')
    )
    latitudeName <- "verbatimLatitude"
    longitudeName <- "verbatimLongitude"
    
    if("decimalLatitude" %in% colnames(dat))
    {
      latitudeName <- "decimalLatitude"
    }
    
    if("decimalLongitude" %in% colnames(dat))
    {
      longitudeName <- "decimalLongitude"
    }
    
    validate(
      need(longitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    validate(
      need(latitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    
    switch (latitudeName,
            "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
            "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
    )
    switch (longitudeName,
            "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
            "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
    )
    map_texture <- input$mapTexture
    map_color <- input$mapColor
    
    future({
      leaflet(
        data = na.omit(
          dat[c(latitudeName, longitudeName)]
        )
      ) %>%
        addProviderTiles(
          map_texture
        ) %>%
        addCircles(
          
          switch(
            longitudeName,
            "decimalLongitude" = ~decimalLongitude,
            "verbatimLongitude" = ~verbatimLongitude
          ),
          switch(
            latitudeName,
            "decimalLatitude" = ~decimalLatitude,
            "verbatimLatitude" = ~verbatimLatitude
          ),
          color = map_color
        ) %>%
        fitBounds(
          switch(
            longitudeName,
            "decimalLongitude" = ~min(decimalLongitude),
            "verbatimLongitude" = ~min(verbatimLongitude)
          ),
          switch(
            latitudeName,
            "decimalLatitude" = ~min(decimalLatitude),
            "verbatimLatitude" = ~min(verbatimLatitude)
          ),
          switch(
            longitudeName,
            "decimalLatitude" = ~max(decimalLongitude),
            "verbatimLatitude" = ~max(verbatimLongitude)
          ),
          switch(
            latitudeName,
            "decimalLatitude" = ~max(decimalLatitude),
            "verbatimLatitude" = ~max(verbatimLatitude)
          )
          
        ) %>%
        leaflet.extras::addDrawToolbar(
          targetGroup='draw',
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          rectangleOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        ) %>%
        addLayersControl(
          overlayGroups = c('draw'),
          options = layersControlOptions(
            collapsed=FALSE
          )
        ) 
    })
    
  })
  
  
  observeEvent(
    input$mymap_draw_new_feature,
    {
      dat <- data_reactive$data
      
      latitudeName <- "verbatimLatitude"
      longitudeName <- "verbatimLongitude"
      
      if("decimalLatitude" %in% colnames(dat))
      {
        latitudeName <- "decimalLatitude"
      }
      
      if("decimalLongitude" %in% colnames(dat))
      {
        longitudeName <- "decimalLongitude"
      }
      
      switch (latitudeName,
              "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
              "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
      )
      switch (longitudeName,
              "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
              "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
      )
      

      data <- na.omit(
        dat[c(
          latitudeName,
          longitudeName
        )]
      )
      cities_coordinates <- SpatialPointsDataFrame(
        data[,c(
          longitudeName,
          latitudeName
        )],
        data
      )
        
      #get the coordinates of the polygon
      polygon_coordinates <- 
        input$mymap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- 
        Polygon(
          do.call(
            rbind,
            lapply(
              polygon_coordinates,
              function(x){c(x[[1]][1],x[[2]][1])}
            )
          )
        )
        
      #use over from the sp package to identify selected cities
      selected_cities <- 
        cities_coordinates %over% 
        SpatialPolygons(
          list(
            Polygons(
              list(
                drawn_polygon
              ),
              "drawn_polygon"
            )
          )
        )
      
      #print the name of the cities
      geo <- as.data.frame(
        dat[which(
          !is.na(
            selected_cities
          )
        ),
        colnames(
          dat
        )]
      )
        
      data_reactive$data <- geo
      data_reactive$leaflet_data <- geo
    
    })
  
  observeEvent(
    input$mymap_draw_deleted_features,
    {
      data_reactive$data <- data_original
      data_reactive$leaflet_data <- NULL
    }
  )
 
}
    
## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
 
