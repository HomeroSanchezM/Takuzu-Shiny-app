library(shiny)

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  uiOutput("matrice_boutons")
  ,
  sidebarLayout(
    sidebarPanel(
     numericInput("nRows", "Nombre de lignes :", 3, min = 1, max = 10),
    numericInput("nCols", "Nombre de colonnes :", 3, min = 1, max = 10),
      actionButton("generate", "Générer la grille")
    ) ,
    mainPanel(
      uiOutput("buttonGrid") 
    )
      )
  
)

server <- function(input, output) {
  output$matrice_boutons <- renderUI({
    nRows <- input$nRows   # Définir le nombre de lignes
    nCols <- input$nCols   # Définir le nombre de colonnes
    req(input$generate) #on doit apuyer sur generate avec de generer la grille pour la premiere fois
    # Créer une matrice de boutons
    boutons <- lapply(1:(nRows * nCols), function(i) {
      actionButton(inputId = paste("bouton", i, sep = "_"), label = paste("0"))
    })
    
    # Organiser les boutons en grille
    tagList(div(class = "btn-grid",
                lapply(split(boutons, ceiling(seq_along(boutons) / nCols)), div, class = "button-row")))
  })
}

shinyApp(ui = ui, server = server)


