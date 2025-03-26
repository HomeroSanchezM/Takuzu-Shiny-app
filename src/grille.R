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
      actionButton("regenerate", "Regénérer la grille"),
      actionButton("verify", "Vérifier")  # Nouveau bouton
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
      background-color: #c5e8fc;
    }
    .btn-correct {
      background-color: #a1d99b;  /* Vert pour les réponses correctes */
    }
  ")
)

server <- function(input, output, session) {
  
  hidden_grid <- reactiveVal()
  user_clicks <- reactiveValues(clicks = NULL)
  verification <- reactiveValues(checked = FALSE)  # Pour suivre si la vérification a été faite
  
  # Créer une version masquée de la grille (50% visible)
  make_hidden_grid <- function() {
    grid <- M
    total_cells <- length(grid)
    show_indices <- sample(1:total_cells, size = round(total_cells * 0.5))
    
    hidden_grid <- matrix(NA, nrow = nrow(grid), ncol = ncol(grid))
    hidden_grid[show_indices] <- grid[show_indices]
    
    return(hidden_grid)
  }
  
  # Initialisation
  hidden_grid(make_hidden_grid())
  user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  verification$checked <- FALSE
  
  # Gestion des clics sur les boutons masqués
  observe({
    lapply(1:nrow(M), function(i) {
      lapply(1:ncol(M), function(j) {
        observeEvent(input[[paste0("btn_", i, "_", j)]], {
          current_grid <- hidden_grid()
          if (is.na(current_grid[i, j])) {
            user_clicks$clicks[i, j] <- (user_clicks$clicks[i, j] + 1) %% 3
            verification$checked <- FALSE  # Réinitialiser la vérification quand on modifie
          }
        })
      })
    })
  })
  
  # Vérification des réponses
  observeEvent(input$verify, {
    verification$checked <- TRUE
  })
  
  # Régénération de la grille
  observeEvent(input$regenerate, {
    hidden_grid(make_hidden_grid())
    user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
    verification$checked <- FALSE
  })
  
  # Révéler la solution complète
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
        user_value <- NULL
        if (user_clicks$clicks[i, j] == 1) user_value <- 1
        if (user_clicks$clicks[i, j] == 2) user_value <- 0
        
        # Déterminer la classe CSS
        btn_class <- if (!is.na(value)) {
          "btn-visible"
        } else if (verification$checked && !is.na(user_value)) {
          if (user_value == M[i, j]) "btn-correct" else "btn-user"
        } else {
          if (user_clicks$clicks[i, j] > 0) "btn-user" else "btn-hidden"
        }
        
        # Déterminer le label
        label <- if (!is.na(value)) {
          as.character(value)
        } else if (user_clicks$clicks[i, j] == 1) {
          "1"
        } else if (user_clicks$clicks[i, j] == 2) {
          "0"
        } else {
          "?"
        }
        
        actionButton(
          inputId = paste0("btn_", i, "_", j),
          label = label,
          class = btn_class,
          width = "50px",
          style = "margin: 2px; height: 50px; font-weight: bold;"
        )
      })
      div(style = "display: flex;", buttons)
    })
    
    div(style = "margin-top: 20px;", rows)
  }
  
  # Afficher la grille
  output$gridUI <- renderUI({
    create_grid(hidden_grid())
  })
}

shinyApp(ui = ui, server = server)