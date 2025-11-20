server <- function(input, output, session) {
  # data init
  ncity=20
  vv <- sample(1:nrow(dataa), nrow(dataa), replace = FALSE)
  dataa <- dataa[vv,][1:ncity,]
  rv <- reactiveValues(
    n = 1, 
    tot = 0,   
    res = data.frame(City = dataa$name, Distance=NA)
  )
  
  # Display map
  output$map <- renderLeaflet({
    leaflet(pays, options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
      setView(lng = -14.548, lat = 14.477, zoom = 7) %>%
      addProviderTiles(provider = providers$CartoDB.VoyagerNoLabels,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0)
  })
  
  # First city
  output$ville <- renderText({
    HTML(paste0("<b>", dataa[1,2],
                " </b><small><small>(", rv$n,"/",ncity,")</small></small>"))
  })
  
  ## Observe mouse clicks 
  observeEvent(input$map_click, {
    # update city 
    output$ville <- renderText({ 
      HTML(paste0("<b>", dataa[rv$n,2], 
                  " </b><small><small>(", rv$n,"/",ncity,")</small></small>"))
    })
    # clean map
    leafletProxy('map') %>% removeMarker(c("1", "2")) 
    
    # update map
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    leafletProxy('map') %>% 
      addMarkers(lng=clng, lat=clat, layerId = "1", icon = icons)
    leafletProxy('map') %>%
      addMarkers(lng=dataa[rv$n, 4], lat=dataa[rv$n, 3], layerId = "2", 
                 icon =icons2) 
    # compute distance
    a <- st_as_sf(data.frame(x = clng,y = clat), 
                  coords = c("x", "y"), crs = 4326)
    b <- st_as_sf(data.frame(x = dataa[rv$n, 4], y = dataa[rv$n, 3]), 
                  coords = c("x", "y"), crs = 4326)
    x <- round(as.numeric(st_distance(a, b)/1000), 0)
    rv$tot <- rv$tot + x
    # Display distance
    showNotification(HTML(paste0("<b>", dataa[rv$n,2],": " , x, " km</b>")), 
                     type = "message", )
    # store distance
    rv$res[rv$n, "Distance"] <- x
    row.names(rv$res) <- rv$res$City
    
    # next city
    rv$n <- rv$n + 1
    
    if(rv$n == ncity + 1 ){
      rv$res <- rv$res[order(rv$res$Distance, decreasing = F),]
      rv$res$Distance <- paste0(rv$res$Distance, " km")
      # Display
      output$x <- renderText({ 
        showModal(
          modalDialog(
            footer = actionButton("rst", "Nouvelle partie!", icon = icon("globe") ),
            title = HTML(paste0("<b>Distance totale : ", rv$tot," km !</b>")),
            size = "s", 
            easyClose = FALSE,
            fade = FALSE,
            label ="resultats",
            renderDT(datatable(
              rv$res[,2,drop=F], colnames = "",
              options = list(lengthChange = FALSE,
                             pageLength = 5,dom="tp")
            ))
          ))
        output$ville <- renderText({""})
        HTML(paste0(""))
      })
    }
    
    observeEvent(input$rst, {
      session$reload()
      return()
    })
    
    
  })
}


