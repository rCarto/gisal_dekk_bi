ui <- fluidPage(
  tags$style(
    type = "text/css",
    "footer{position: absolute; bottom:1%; left: 1%; padding:5px;}"
  ),
  fluidRow(
    column(
      width = 12,
      h4("Guess the city location"),
      htmlOutput("ville"),
      htmlOutput("x")
    ),
    column(
      width = 8,
      leafletOutput("map", height = "75vh"),
    ),
    HTML(
      "<footer>
         <a href='https://riate.cnrs.fr'><img src='logo_footer.svg' height='25px'></a>
       </footer>"
    )
  )
)