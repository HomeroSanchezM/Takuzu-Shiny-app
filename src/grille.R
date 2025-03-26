library(shiny)

# Charger la matrice M depuis le fichier sauvegardé
M <- readRDS("matrice_M.rds")

# Exclure la 11ème ligne et la 11ème colonne
M <- M[1:10, 1:10]

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  uiOutput("matrice_boutons"),
  sidebarLayout(
    sidebarPanel(
      textOutput("rules"), 
      numericInput("nRows", "Nombre de lignes :", 10, min = 1, max = 10),
      numericInput("nCols", "Nombre de colonnes :", 10, min = 1, max = 10),
      actionButton("generate", "Générer la grille")
    ),
    mainPanel(
      uiOutput("buttonGrid")
    )
  )
)

server <- function(input, output) {
  
  output$rules <- renderText({
    " Règles du jeu  \n
— chaque case de la grille doit être remplie avec un 0 ou un 1 ;  \n
— chaque ligne et chaque colonne doivent contenir autant de 0 que de 1  \n
— il est interdit d’avoir trois 0 ou trois 1 consécutifs dans une ligne ou une colonne ;  \n
— deux lignes ou deux colonnes identiques sont interdites dans la même grille.  \n
 \n
Stratégies pour résoudre un Takuzu  \n
— délecter les triples : si deux 0 ou deux 1 se suivent, la case suivante doit forcément
contenir l’autre chiffre ;  \n
— équilibrer les 0 et les 1 : une ligne ou une colonne ne peut pas contenir plus de la
moitié des cases d’un même chiffre.  \n
— comparer les lignes et colonnes déjà complétées : si une ligne ou une colonne est
presque remplie et qu’une autre est similaire, il faut ajuster les chiffres pour éviter
les doublons.  \n "  
  })
  
  output$matrice_boutons <- renderUI({
    req(input$generate) # On doit appuyer sur "Générer la grille" pour la première fois
    
    # Créer une matrice de boutons en utilisant les valeurs de M
    boutons <- lapply(1:100, function(i) {
      row <- ceiling(i / 10)
      col <- ifelse(i %% 10 == 0, 10, i %% 10)
      value <- M[row, col]
      actionButton(inputId = paste("bouton", i, sep = "_"), label = as.character(value))
    })
    
    # Organiser les boutons en grille
    tagList(div(class = "btn-grid",
                lapply(split(boutons, ceiling(seq_along(boutons) / 10), div, class = "button-row"))))
  })
}

shinyApp(ui = ui, server = server)