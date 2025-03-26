library(shiny)

# Charger la matrice M depuis le fichier sauvegardé
M <- readRDS("matrice_M.rds")

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  sidebarLayout(
    sidebarPanel(
      h3("Règles du jeu"),
      tags$ul(
        tags$li("Chaque case doit contenir 0 ou 1"),
        tags$li("Autant de 0 que de 1 par ligne/colonne"),
        tags$li("Pas trois 0 ou 1 consécutifs"),
        tags$li("Pas deux lignes/colonnes identiques")
      ),
      actionButton("generate", "Afficher la grille")
    ),
    mainPanel(
      uiOutput("gridUI")
    )
  )
)

server <- function(input, output) {
  
  # Réactive pour stocker la matrice
  grid_data <- reactiveVal(M)
  
  # Observer le bouton de génération
  observeEvent(input$generate, {
    grid_data(M)
  })
  
  # Génération de la grille de boutons
  output$gridUI <- renderUI({
    req(input$generate) # Attendre que le bouton soit cliqué
    
    M <- grid_data()
    n_rows <- nrow(M)
    n_cols <- ncol(M)
    
    # Créer une liste de lignes
    rows <- lapply(1:n_rows, function(i) {
      # Créer une ligne de boutons
      buttons <- lapply(1:n_cols, function(j) {
        value <- M[i, j]
        actionButton(
          inputId = paste0("btn_", i, "_", j),
          label = as.character(value),
          width = "50px",
          style = "margin: 2px; height: 50px; font-weight: bold;"
        )
      })
      # Retourner la ligne dans un div
      div(style = "display: flex;", buttons)
    })
    
    # Retourner toutes les lignes
    div(style = "margin-top: 20px;", rows)
  })
}

shinyApp(ui = ui, server = server)