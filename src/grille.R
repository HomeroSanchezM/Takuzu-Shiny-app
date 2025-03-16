library(shiny)

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  uiOutput("matrice_boutons")
  ,
  sidebarLayout(
    sidebarPanel(
      textOutput("rules"), 
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
  
 
  output$rules <- renderText({  # Affichage des rules
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


