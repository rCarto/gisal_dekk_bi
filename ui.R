ui <- fluidPage(
  tags$style(
    type = "text/css",
    "footer{position: absolute; bottom:1%; left: 1%; padding:5px;}"
  ),
  fluidRow(
    column(
      width = 12,
      h4("Devinez la position de la ville !"),
      htmlOutput("ville"),
      htmlOutput("x")
    ),
    column(
      width = 8,
      leafletOutput("map", height = "75vh"),
    ),
    HTML(
      "<footer>
         <a href='https://github.com/rCarto/gisal_dekk_bi/'><img src='logo_footer.svg' height='25px'></a>
       </footer>"
    )
  )
)