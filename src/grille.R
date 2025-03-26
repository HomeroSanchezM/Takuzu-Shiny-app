library(shiny)

# Charger la matrice M depuis le fichier sauvegardé
M <- readRDS("matrice_M.rds")

ui <- fluidPage(
  titlePanel("Jeu Takuzu - Version Simplifiée"),
  sidebarLayout(
    sidebarPanel(
      h3("Règles du jeu"),
      tags$ul(
        tags$li("Complétez la grille avec des 0 et des 1"),
        tags$li("Même nombre de 0 et de 1 par ligne/colonne"),
        tags$li("Pas trois 0 ou 1 côte à côte"),
        tags$li("Pas deux lignes/colonnes identiques")
      ),
      actionButton("reveal", "Révéler la solution"),
      actionButton("regenerate", "Regénérer la grille")
    ),
    mainPanel(
      uiOutput("gridUI")
    )
  ),
  tags$style("
    .btn-hidden {
      background-color: #f0f0f0;
      color: transparent;
    }
    .btn-visible {
      background-color: white;
    }
    .btn-user {
      background-color: #6baed6;
    }
  ")
)

server <- function(input, output, session) {
  
  hidden_grid <- reactiveVal()
  
  #enregistrement des clicks
  user_clicks <- reactiveValues(clicks = NULL)
  # Créer une version masquée de la grille (50% visible)
  make_hidden_grid <- function() {
    # Créer une copie de la matrice
    grid <- M
    
    # Sélection aléatoire de 50% des cases à montrer
    total_cells <- length(grid)
    show_indices <- sample(1:total_cells, size = round(total_cells * 0.5))
    
    # Masquer les autres cases
    hidden_grid <- matrix(NA, nrow = nrow(grid), ncol = ncol(grid))
    hidden_grid[show_indices] <- grid[show_indices]
    
    return(hidden_grid)
  }
  
  # Initialisation grille masqué
  hidden_grid(make_hidden_grid())
  
  # Initialisation grille de click
  user_clicks$clicks <- matrix(FALSE, nrow = nrow(M), ncol = ncol(M))
  
  # Gestion des clics sur les boutons masqués
  observe({
    lapply(1:nrow(M), function(i) {
      lapply(1:ncol(M), function(j) {
        observeEvent(input[[paste0("btn_", i, "_", j)]], {
          current_grid <- hidden_grid()
          if (is.na(current_grid[i, j]) && !user_clicks$clicks[i, j]) {
            user_clicks$clicks[i, j] <- TRUE
          }
        })
      })
    })
  })
  
  # Afficher la régénération de la grille
   observeEvent(input$regenerate, {
    hidden_grid(make_hidden_grid())
    user_clicks$clicks <- matrix(FALSE, nrow = nrow(M), ncol = ncol(M))
  })
  
  # Afficher la solution complète quand on clique sur le bouton
  observeEvent(input$reveal, {
    hidden_grid(M)
    })
  
  
  # Fonction pour créer la grille UI
  create_grid <- function(grid_data) {
    n_rows <- nrow(grid_data)
    n_cols <- ncol(grid_data)
    
    rows <- lapply(1:n_rows, function(i) {
      buttons <- lapply(1:n_cols, function(j) {
        value <- grid_data[i, j]
        if (is.na(value)) {
          if (user_clicks$clicks[i, j]) {
            # Case masquée cliquée - affiche 1
            actionButton(
              inputId = paste0("btn_", i, "_", j),
              label = "1",
              class = "btn-user",
              width = "50px",
              style = "margin: 2px; height: 50px;"
            )
          } else {
            # Case masquée
            actionButton(
              inputId = paste0("btn_", i, "_", j),
              label = "?",
              class = "btn-hidden",
              width = "50px",
              style = "margin: 2px; height: 50px;"
            )
          }
        } else {
          # Case visible
          actionButton(
            inputId = paste0("btn_", i, "_", j),
            label = as.character(value),
            class = "btn-visible",
            width = "50px",
            style = "margin: 2px; height: 50px; font-weight: bold;"
          )
        }
      })
      div(style = "display: flex;", buttons)
    })
    
    div(style = "margin-top: 20px;", rows)
  }
  
  # Afficher la grille initiale (50% masquée)
  output$gridUI <- renderUI({
    create_grid(hidden_grid())
  })
}

shinyApp(ui = ui, server = server)