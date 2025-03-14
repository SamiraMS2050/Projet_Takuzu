# Charger les bibliothèques nécessaires
library(shiny)
library(Rcpp)

# ---- 1️⃣ Générer une grille Takuzu en C++ ----
cppFunction('
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix generateTakuzu(int size) {
    IntegerMatrix grid(size, size);
    for (int row = 0; row < size; row++) {
        for (int col = 0; col < size; col++) {
            grid(row, col) = rand() % 2;  // Remplissage aléatoire 0 ou 1
        }
    }
    return grid;
}
')

# ---- 2️⃣ Interface Utilisateur (UI) ----
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

# ---- 3️⃣ Serveur ----
server <- function(input, output, session) {
  size <- 8  # Taille de la grille
  
  # Générer une nouvelle grille Takuzu au démarrage
  takuzu_grid <- reactiveVal(generateTakuzu(size))
  
  # ---- 3️⃣.1 Afficher la grille ----
  output$takuzu_ui <- renderUI({
    grid <- takuzu_grid()
    
    # Création des boutons interactifs
    buttons <- lapply(1:size, function(row) {
      lapply(1:size, function(col) {
        actionButton(inputId = paste0("cell_", row, "_", col),
                     label = as.character(grid[row, col]),  # Afficher 0 ou 1
                     class = "takuzu-btn")
      })
    })
    
    # Affichage sous forme de tableau HTML
    do.call(fluidRow, lapply(buttons, function(row) {
      column(12, do.call(splitLayout, c(row, list(cellWidths = rep("40px", size)))))
    }))
  })
  
  # ---- 3️⃣.2 Gérer les clics de l'utilisateur ----
  observe({
    for (row in 1:size) {
      for (col in 1:size) {
        local({
          r <- row
          c <- col
          observeEvent(input[[paste0("cell_", r, "_", c)]], {
            grid <- isolate(takuzu_grid())  # Ne pas recalculer toute la grille
            grid[r, c] <- ifelse(grid[r, c] == 0, 1, 0)  # Inversion 0 ⇄ 1
            takuzu_grid(grid)  # Mise à jour réactive
            
            # Mise à jour de l'affichage du bouton
            updateActionButton(session, paste0("cell_", r, "_", c), label = as.character(grid[r, c]))
          }, ignoreNULL = TRUE)
        })
      }
    }
  })
  
  # ---- 3️⃣.3 Vérifier la solution et indiquer les erreurs ----
  observeEvent(input$check_solution, {
    grid <- takuzu_grid()
    
    invalid_rows <- c()
    invalid_cols <- c()
    
    # Vérifier chaque ligne et colonne
    for (i in 1:size) {
      # Règle 1: Chaque ligne et colonne doit avoir 50% de 0 et 50% de 1
      if (sum(grid[i, ] == 0) != size / 2 || sum(grid[i, ] == 1) != size / 2) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (sum(grid[, i] == 0) != size / 2 || sum(grid[, i] == 1) != size / 2) {
        invalid_cols <- c(invalid_cols, i)
      }
      
      # Règle 2: Pas plus de 2 chiffres identiques consécutifs dans une ligne ou colonne
      if (any(rle(grid[i, ])$lengths >= 3)) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (any(rle(grid[, i])$lengths >= 3)) {
        invalid_cols <- c(invalid_cols, i)
      }
    }
    
    # Génération du message d'erreur
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
  
  # ---- 3️⃣.4 Générer une nouvelle grille ----
  observeEvent(input$new_game, {
    takuzu_grid(generateTakuzu(size))  # Nouvelle grille
  })
}

# ---- 4️⃣ Lancer l'application ----
shinyApp(ui, server)
