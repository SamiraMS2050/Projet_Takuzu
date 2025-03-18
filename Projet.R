library(Rcpp)
sourceCpp("takuzu.cpp")  # Assurez-vous que ce fichier est bien présent
library(shiny)

# ---- Interface Utilisateur (UI) ----
ui <- fluidPage(
  titlePanel("Jeu Takuzu - Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("new_game", "Nouvelle Grille"),
      actionButton("check_solution", "Vérifier la solution"),
      textOutput("result")
    ),
    
    mainPanel(
      uiOutput("takuzu_ui")  # Grille interactive
    )
  )
)

# ---- Serveur ----
server <- function(input, output, session) {
  size <- 8  # Taille de la grille
  fillRate <- 0.4  # Pourcentage de cases pré-remplies
  
  # Grille réactive
  initial_grid <- generateTakuzu(size, fillRate)
  takuzu_grid <- reactiveVal(initial_grid)
  
  # ---- Affichage de la grille ----
  output$takuzu_ui <- renderUI({
    grid <- takuzu_grid()
    
    buttons <- lapply(1:size, function(row) {
      lapply(1:size, function(col) {
        cell_value <- grid[row, col]
        inputId <- paste0("cell_", row, "_", col)
        
        actionButton(
          inputId = inputId,
          label = ifelse(cell_value == -1, " ", as.character(cell_value)),  # Afficher case vide
          class = "takuzu-btn",
          disabled = (initial_grid[row, col] != -1)  # Bloquer cases pré-remplies
        )
      })
    })
    
    # Affichage sous forme de tableau HTML
    do.call(fluidRow, lapply(buttons, function(row) {
      column(12, do.call(splitLayout, c(row, list(cellWidths = rep("40px", size)))))
    }))
  })
  
  # ---- Gestion des clics de l'utilisateur ----
  observe({
    for (row in 1:size) {
      for (col in 1:size) {
        local({
          r <- row
          c <- col
          input_id <- paste0("cell_", r, "_", c)
          
          observeEvent(input[[input_id]], {
            grid <- isolate(takuzu_grid())  
            
            # Seules les cases vides doivent être modifiables
            if (initial_grid[r, c] == -1) {  
              grid[r, c] <- ifelse(grid[r, c] == 1, 0, 1)  # Alternance 0 ⇄ 1
              takuzu_grid(grid)  
              
              # Mettre à jour l'affichage du bouton
              updateActionButton(session, input_id, label = as.character(grid[r, c]))
            }
          }, ignoreNULL = TRUE)
        })
      }
    }
  })
  
  # ---- Vérification de la solution ----
  observeEvent(input$check_solution, {
    grid <- takuzu_grid()
    
    invalid_rows <- c()
    invalid_cols <- c()
    
    # Vérifier chaque ligne et colonne
    for (i in 1:size) {
      # Règle 1: Chaque ligne et colonne doit contenir 50% de 0 et 50% de 1
      if (sum(grid[i, ] == 0) != size / 2 || sum(grid[i, ] == 1) != size / 2) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (sum(grid[, i] == 0) != size / 2 || sum(grid[, i] == 1) != size / 2) {
        invalid_cols <- c(invalid_cols, i)
      }
      
      # Règle 2: Pas plus de 2 chiffres identiques consécutifs
      if (any(rle(grid[i, ])$lengths >= 3)) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (any(rle(grid[, i])$lengths >= 3)) {
        invalid_cols <- c(invalid_cols, i)
      }
    }
    
    # Génération du message de validation
    if (length(invalid_rows) == 0 && length(invalid_cols) == 0) {
      output$result <- renderText("🎉 Bravo ! Grille correcte !")
    } else {
      message <- "❌ Erreur dans la grille :\n"
      if (length(invalid_rows) > 0) {
        message <- paste0(message, "Lignes incorrectes : ", paste(invalid_rows, collapse = ", "), "\n")
      }
      if (length(invalid_cols) > 0) {
        message <- paste0(message, "Colonnes incorrectes : ", paste(invalid_cols, collapse = ", "))
      }
      output$result <- renderText(message)
    }
  })
  
  # ---- Générer une nouvelle grille ----
  observeEvent(input$new_game, {
    new_grid <- generateTakuzu(size, fillRate)
    takuzu_grid(new_grid)  # Mettre à jour la grille
    initial_grid <<- new_grid  # Mémoriser les nouvelles valeurs initiales
  })
}

# ---- Lancer l'application ----
shinyApp(ui, server)
