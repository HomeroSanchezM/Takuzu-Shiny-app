#' Lance l'interface Shiny pour jouer au Takuzu
#'
#' @export
play_takuzu <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Le package shiny est nécessaire pour cette fonction. Installez-le avec install.packages('shiny')")
  }
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Jeu Takuzu - Version Simplifiée"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("Règles du jeu"),
        shiny::tags$ul(
          shiny::tags$li("Complétez la grille avec des 0 et des 1"),
          shiny::tags$li("Même nombre de 0 et de 1 par ligne/colonne"),
          shiny::tags$li("Pas trois 0 ou 1 côte à côte"),
          shiny::tags$li("Pas deux lignes/colonnes identiques")
        ),
        shiny::actionButton("reveal", "Révéler la solution"),
        shiny::actionButton("regenerate_easy", "Niveau facile (80% visible)"),
        shiny::actionButton("regenerate_normal", "Niveau normal (50% visible)"),
        shiny::actionButton("regenerate_hard", "Niveau difficile (30% visible)"),
        shiny::actionButton("verify", "Vérifier")
      ),
      shiny::mainPanel(
        shiny::uiOutput("gridUI")
      )
    ),
    shiny::tags$style("
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
        background-color: #a1d99b;
      }
    ")
  )
  
  server <- function(input, output, session) {
    size <- 10
    M <- generate_takuzu_grid(size)
    
    hidden_grid <- shiny::reactiveVal()
    user_clicks <- shiny::reactiveValues(clicks = NULL)
    verification <- shiny::reactiveValues(checked = FALSE)
    
    make_hidden_grid <- function(percentage_visible) {
      grid <- M
      total_cells <- length(grid)
      show_indices <- sample(1:total_cells, size = round(total_cells * percentage_visible))
      
      hidden_grid <- matrix(NA, nrow = nrow(grid), ncol = ncol(grid))
      hidden_grid[show_indices] <- grid[show_indices]
      
      return(hidden_grid)
    }
    
    hidden_grid(make_hidden_grid(0.5))
    user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
    verification$checked <- FALSE
    
    shiny::observe({
      lapply(1:nrow(M), function(i) {
        lapply(1:ncol(M), function(j) {
          shiny::observeEvent(input[[paste0("btn_", i, "_", j)]], {
            current_grid <- hidden_grid()
            if (is.na(current_grid[i, j])) {
              user_clicks$clicks[i, j] <- (user_clicks$clicks[i, j] + 1) %% 3
              verification$checked <- FALSE
            }
          })
        })
      })
    })
    
    shiny::observeEvent(input$verify, {
      verification$checked <- TRUE
    })
    
    shiny::observeEvent(input$regenerate_easy, {
      hidden_grid(make_hidden_grid(0.8))
      user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
      verification$checked <- FALSE
    })
    
    shiny::observeEvent(input$regenerate_normal, {
      hidden_grid(make_hidden_grid(0.5))
      user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
      verification$checked <- FALSE
    })
    
    shiny::observeEvent(input$regenerate_hard, {
      hidden_grid(make_hidden_grid(0.3))
      user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
      verification$checked <- FALSE
    })
    
    shiny::observeEvent(input$reveal, {
      hidden_grid(M)
    })
    
    create_grid <- function(grid_data) {
      n_rows <- nrow(grid_data)
      n_cols <- ncol(grid_data)
      
      rows <- lapply(1:n_rows, function(i) {
        buttons <- lapply(1:n_cols, function(j) {
          value <- grid_data[i, j]
          user_value <- NULL
          if (user_clicks$clicks[i, j] == 1) user_value <- 1
          if (user_clicks$clicks[i, j] == 2) user_value <- 0
          
          btn_class <- if (!is.na(value)) {
            "btn-visible"
          } else if (verification$checked && !is.na(user_value)) {
            if (user_value == M[i, j]) "btn-correct" else "btn-user"
          } else {
            if (user_clicks$clicks[i, j] > 0) "btn-user" else "btn-hidden"
          }
          
          label <- if (!is.na(value)) {
            as.character(value)
          } else if (user_clicks$clicks[i, j] == 1) {
            "1"
          } else if (user_clicks$clicks[i, j] == 2) {
            "0"
          } else {
            "?"
          }
          
          shiny::actionButton(
            inputId = paste0("btn_", i, "_", j),
            label = label,
            class = btn_class,
            width = "50px",
            style = "margin: 2px; height: 50px; font-weight: bold;"
          )
        })
        shiny::div(style = "display: flex;", buttons)
      })
      shiny::div(style = "margin-top: 20px;", rows)
    }
    
    output$gridUI <- shiny::renderUI({
      create_grid(hidden_grid())
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}