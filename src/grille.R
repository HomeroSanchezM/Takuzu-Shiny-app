library(shiny)



#initialisation, taille de la grille
size = 10
#matrice des vrais valeurs vide avec colonne et ligne mortes en bouts pour simplifier fonctions test
M <- matrix(
  data = NA,
  nrow = size+1,
  ncol = size+1,
  byrow = TRUE,
  dimnames = NULL
)

a <- 0 #paramètre de décalage pour refaire à partir d'un certain rang, utile ?

#correction verticale ligne ou colonne/cases du dessus ou dessous;
#écriture + concise en piquant directement dans M
testL_V<-function(t,i,M) {
  #il faut t > 2 sinon <0 = erreur. Pour 00_ 
  if (sum(M[(t-2):(t-1), i], na.rm = TRUE) == 0) {
    M[t, i] <- 1
  }
  #pour les 0_0 et 1_1 si t+1 existe
  if(is.na(M[t+1,i]) == FALSE) {
    if(sum(M[c(t-1,t+1), i], na.rm = TRUE) == 0) {
      M[t, i] <- 1
    }
    if(sum(M[c(t-1,t+1), i], na.rm = TRUE) == 2) {
      M[t, i] <- 0
    }}
  #Pour les 11_
  if (sum(M[(t-2):(t-1), i], na.rm = TRUE) == 2) {
    M[t, i] <- 0
  }
  return(M)
}

#correction horizontale : ligne/cases précédentes ou suivante
testL_H<-function(t,i,M) {
  #il faut i>2. Pour 00_
  if (sum(M[t,(i-2):(i-1)], na.rm = TRUE) == 0) {
    M[t,i] <- 1
  }
  #Pour 0_0 et 1_1 si i+1 existe
  if(is.na(M[t,i+1]) == FALSE) {
    if(sum(M[t,c(i-1,i+1)], na.rm = TRUE) == 0) {
      M[t, i] <- 1
    }
    if(sum(M[t,c(i - 1,i + 1)], na.rm = TRUE) == 2) {
      M[t, i] <- 0
    }}
  #pour 11_
  if (sum(M[t,(i - 2):(i - 1)], na.rm = TRUE) == 2) {
    M[t, i] <- 0
  }
  return(M)
}

#accepte un vecteur, étant la ligne ou colonne et renvoie un 0/1 dépendant de ce qui reste
fill<-function(V) {
  #Compte les 0 et 1 déjà présents
  n1 <- sum(V == 1, na.rm = TRUE)
  n0 <- sum(V == 0, na.rm = TRUE)
  print(c(n1,n0))
  if(n1 > size/2) return(0) #le nombre de 0/1 pourrait être faux
  if(n0 > size/2) return(1) #et entrainera la répétition de la rangée à la fin
  #vecteur des valeurs de références (restants) actualisées
  ref <- c(rep(c(1), (size / 2) - n1), rep(c(0), (size / 2) - n0))
  print(ref)
  #le if évite un erreur si ref est nul
  if (length(ref) == 0) {
    return(NA) #un NA dans ce cas
  } 
  ref<-sample(ref, size = length(ref), replace = FALSE) #on mélange les 0/1 
  #choix d'une valeur parmi celles restantes
  return(sample(ref, size = 1, replace = FALSE))
}

#vérification finale, en fin de ligne ou colonne : on efface et refait à partir du rang j, sinon au rang j-1 ?
End_testL<-function(j,M) {
  #problème de 1 ou 0 en ligne
  if(((sum(M[j,], na.rm = TRUE) == 1 || sum(M[j,], na.rm = TRUE) == 0) != size/2) & ((sum(M[j,], na.rm = TRUE) == 1 & sum(M[j,], na.rm = TRUE) == 0) != size/2)) 
  {
    M[j,j:size]<-c(rep(NA, size+1-j))
    M <- ligne(j,0, M)
  }
  return(M)
}

End_testC<-function(j,M) {  
  #en colonne
  if(((sum(M[,j], na.rm = TRUE) == 1 || sum(M[,j], na.rm = TRUE) == 0) != size/2)  & ((sum(M[,j], na.rm = TRUE) == 1 & sum(M[,j], na.rm = TRUE) == 0) != size/2)) {
    M[(j+1):size,j]<-c(rep(NA, size-j))
    M <- colonne(j,0, M)
  }
  return(M)
}

ligne <- function(x, a, M) {
  # Etape déterministe
  #A partir de ligne 3, appli des contraintes en colonne sur toute la ligne avant de la compléter
  if(x>2) {
    for (i in seq(from = x + a, to = size, by = 1)) {
      M<-testL_V(x,i,M)
    }}
  
  # on remplit le reste de la ligne respectant les contraintes horizontales dans les cases vides
  for (i in seq(from = x + a, to = size, by = 1)) {
    if(i<= 2) {
      v <- M[x,]
      M[x,i]<-fill(v)
    }
    if(i>2) {
      #testL_OO_11() 
      if(is.na(M[x, i]) == TRUE) {
        #on applique la règle horizontalement
        M<-testL_H(x,i,M)
        #s'il n'y a pas eu d'action, toujours Na 
        if(is.na(M[x, i]) == TRUE) {
          #un vecteur qui est la ligne en cour
          v <-M[x,]
          #if(is.na(fill(v)) == TRUE) {
          #  M[x,x:size]<-c(rep(NA, size+1-x))
          # M <- ligne(x,0, M)
          # break
          #} else {
          M[x, i] <-fill(v) }
        #}
      }}}
  return(M)
}

colonne <- function(x, a, M) {
  # Etape déterministe :
  #A partir de colonne 3, application des contraintes en ligne sur toute la colonne avant de la compléter (déterminiiste)
  #on utilise les fonctions de la ligne en inversant
  if(x>2) {
    for (i in seq(from = x + 1 + a, to = size, by = 1)) {
      M<-testL_H(i,x,M)
    }
  }  
  # on remplit le reste de la colonne respectant les contraintes verticales dans les cases vides
  for (i in seq(from = x + 1 + a, to = size, by = 1)) {
    if(i<=2) {
      #on extrait la colonne x, pour compter les 0 et 1.
      v <- M[,x]
      M[i, x] <-fill(v) 
    }
    if(i>2) { 
      #test_OO_11() 
      if(is.na(M[i, x]) == TRUE) {
        #on applique la règle verticale
        M<-testL_V(i,x,M)
        #s'il n'y a pas eu d'action, toujours Na 
        if(is.na(M[i, x]) == TRUE) {
          #détection des 1 et 0 déjà présents sur toute la colonne et choix aléatoire 0/1
          v<-M[,x]
          #if(is.na(fill(v)) == TRUE) {
          #    M[(x+1):size,x]<-c(rep(NA, size-x))
          #  M<-colonne(x+1,0, M)
          #   break
          # } else {
          M[i, x] <-fill(v)}
        # }
      }
    }
  }
  return(M)
}



#On commence par la ligne 1 puis la colonne 1 avec x=1
for (j in c(1:size)) {
  if (j <= size-1)  {
    print(j)
    M <- ligne(j,0, M)
    M<-End_testL(j,M)
    M <- colonne(j,0, M)
    M<-End_testC(j,M)
  } else M <- ligne(j,0, M)
}


M
sum(M[size,], na.rm = TRUE)
sum(M[,size], na.rm = TRUE)




#fonction de correction si conflit OO/11 : pas fini
test.OO_11<-function (t,i,M) {
  #correction si incompatible ligne avec colonne et par 2x2
  if (i > 2 & t > 2) {
    #Cas1: H:00 & V:11 : permutation au choix
    if (sum(M[t, i-2:i-1], na.rm = TRUE) == 0 & sum(M[t-2:t-1, i], na.rm = TRUE) == 2) {
      #Si les valeurs au dessus dans la colonne à gauche ne sont pas aussi V:11, permutation H
      if (sum(M[t-2:t-1, i-1]) != 2) {
        M[t, i - 1] <- 1
        M[t, i] <- 0
        #on rééchantillonne la colonne x-1 quand elle existe et en dessous de la correction
        if (i == t) {
          colonne(t - 1, 1, M)
          a=0
        }
      }
      #Si les valeurs au dessus dans la colonne à gauche sont aussi V:11, permutation V
      if ((M[x - 1, i - 1] + M[x - 2, i - 1]) == 2) {
        M[x - 1, i] <- 0
        M[x, i] <- 1
        #on rééchantillonne la ligne x-1 au dessous au delà de la correction
        ligne(x - 1, 2, M)
        a=0
      }
      
    }
    #Cas2: H:11 & V:00 : permutation au choix
    if ((M[x, i - 1] + M[x, i - 2]) == 2 &
        (M[x - 1, i] + M[x - 2, i]) == 0) {
      #Si les valeurs au dessus dans la colonne à gauche ne sont pas aussi V:00, permutation H
      if ((M[x - 1, i - 1] + M[x - 2, i - 1]) != 0) {
        M[x, i - 1] <- 0
        M[x, i] <- 1
        #on rééchantillonne la colonne x-1 quand elle existe et en dessous de la correction
        if (i == x) {
          colonne(x - 1, 1, M)
          a=0
        }
      }
      #Si les valeurs au dessus dans la colonne à gauche sont aussi V:00, permutation V
      if ((M[x - 1, i - 1] + M[x - 2, i - 1]) == 0) {
        M[x - 1, i] <- 1
        M[x, i] <- 0
        #on rééchantillonne la ligne x-1 au dessus et au delà de la correction
        ligne(x-1, 2, M)
        a=0
      }
      
    }
  } 
}


# Exclure la dernière ligne et la dernière colonne
M_final <- M[1:size, 1:size]












###################################################################
###################################################################

# Charger la matrice M depuis le fichier sauvegardé
M <- M_final

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
      actionButton("regenerate_easy", "Niveau facile (80% visible)"),
      actionButton("regenerate_normal", "Niveau normal (50% visible)"),
      actionButton("regenerate_hard", "Niveau difficile (30% visible)"),
      actionButton("verify", "Vérifier")
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
  verification <- reactiveValues(checked = FALSE)
  
  # Fonction pour créer une grille avec un pourcentage de cases visibles
  make_hidden_grid <- function(percentage_visible) {
    grid <- M
    total_cells <- length(grid)
    show_indices <- sample(1:total_cells, size = round(total_cells * percentage_visible))
    
    hidden_grid <- matrix(NA, nrow = nrow(grid), ncol = ncol(grid))
    hidden_grid[show_indices] <- grid[show_indices]
    
    return(hidden_grid)
  }
  
  # Initialisation avec niveau normal (50%)
  hidden_grid(make_hidden_grid(0.5))
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
            verification$checked <- FALSE
          }
        })
      })
    })
  })
  
  # Vérification des réponses
  observeEvent(input$verify, {
    verification$checked <- TRUE
  })
  
  # Régénération de la grille - Niveau facile (80%)
  observeEvent(input$regenerate_easy, {
    hidden_grid(make_hidden_grid(0.8))
    user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
    verification$checked <- FALSE
  })
  
  # Régénération de la grille - Niveau normal (50%)
  observeEvent(input$regenerate_normal, {
    hidden_grid(make_hidden_grid(0.5))
    user_clicks$clicks <- matrix(0, nrow = nrow(M), ncol = ncol(M))
    verification$checked <- FALSE
  })
  
  # Régénération de la grille - Niveau difficile (30%)
  observeEvent(input$regenerate_hard, {
    hidden_grid(make_hidden_grid(0.3))
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