library(Rcpp)
sourceCpp("takuzu.cpp")  # Utilise votre fonction generateTakuzu existante
library(shiny)

# ---- Interface Utilisateur (UI) ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .takuzu-btn {
        width: 40px !important;
        height: 40px !important;
        margin: 1px !important;
        font-weight: bold;
        font-size: 16px;
      }
      .incorrect-cell {
        background-color: #ffcccc !important;
        color: black !important;
      }
      .hint-cell {
        background-color: #cccccc !important;
        color: black !important;
      }
      .zero-cell {
        color: blue !important;
      }
      .one-cell {
        color: black !important;
      }
    "))
  ),
  titlePanel("Jeu Takuzu - Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("new_game", "Nouvelle Grille", class = "btn-primary btn-block"),
      hr(),
      
      # Vérification de ligne/colonne spécifique ou grille complète
      wellPanel(
        h4("Vérifier"),
        selectInput("check_type", "Type:", choices = c("Ligne" = "row", "Colonne" = "col", "Grille" = "grid")),
        uiOutput("line_number_selector"),
        actionButton("check_line", "Vérifier", class = "btn-info btn-block")
      ),
      
      # Bouton pour obtenir un indice
      actionButton("get_hint", "Obtenir un indice", class = "btn-warning btn-block"),
      
      # Bouton pour afficher la solution
      hr(),
      actionButton("show_solution", "Afficher la solution", class = "btn-danger btn-block"),
      
      hr(),
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
  
  # Grilles réactives
  initial_grid <- reactiveVal(NULL)
  takuzu_grid <- reactiveVal(NULL)
  solution_grid <- reactiveVal(NULL)  # Pour stocker la solution complète
  hint_cells <- reactiveVal(NULL)  # Pour suivre les indices donnés
  valid_rows <- reactiveVal(NULL)  # Pour suivre les lignes valides
  valid_cols <- reactiveVal(NULL)  # Pour suivre les colonnes valides
  
  # NOUVEAU: Réactifs pour les erreurs
  error_cells <- reactiveVal(matrix(FALSE, nrow = size, ncol = size))
  
  # Fonction pour vérifier si une valeur est valide à une position donnée
  isValidPlacement <- function(grid, row, col, val) {
    # Vérifier pas plus de deux chiffres identiques consécutifs horizontalement
    if (col > 2) {
      if (grid[row, col-1] == val && grid[row, col-2] == val) {
        return(FALSE)
      }
    }
    
    # Vérifier pas plus de deux chiffres identiques consécutifs verticalement
    if (row > 2) {
      if (grid[row-1, col] == val && grid[row-2, col] == val) {
        return(FALSE)
      }
    }
    
    # Vérifier l'équilibre des 0 et 1 dans la ligne
    zero_count <- sum(grid[row, ] == 0, na.rm = TRUE)
    if (val == 0 && zero_count >= size/2) {
      return(FALSE)
    }
    
    # Vérifier l'équilibre des 0 et 1 dans la colonne
    zero_count <- sum(grid[, col] == 0, na.rm = TRUE)
    if (val == 0 && zero_count >= size/2) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Fonction pour vérifier si une ligne ou colonne est valide
  isValidLine <- function(line) {
    # Vérifier si la ligne est complète
    if (any(line == -1)) return(FALSE)
    
    # Règle 1: Nombre égal de 0 et 1
    if (sum(line == 0) != sum(line == 1)) return(FALSE)
    
    # Règle 2: Pas plus de 2 chiffres identiques consécutifs
    runs <- rle(line)
    if (any(runs$lengths > 2)) return(FALSE)
    
    return(TRUE)
  }
  
  # NOUVEAU: Fonction pour identifier les erreurs dans une ligne ou colonne
  findErrorsInLine <- function(line) {
    if (any(line == -1)) return(NULL)  # Ligne incomplète
    
    errors <- rep(FALSE, length(line))
    
    # Vérifier les séquences de plus de 2 consécutifs
    runs <- rle(line)
    start_idx <- 1
    for (i in 1:length(runs$lengths)) {
      if (runs$lengths[i] > 2) {
        run_length <- runs$lengths[i]
        for (j in start_idx:(start_idx + run_length - 1)) {
          errors[j] <- TRUE
        }
      }
      start_idx <- start_idx + runs$lengths[i]
    }
    
    # Si le nombre de 0 et 1 n'est pas équilibré
    zero_count <- sum(line == 0)
    one_count <- sum(line == 1)
    
    if (zero_count > size/2) {
      # Trop de 0, marquer les 0 comme erreurs
      errors[line == 0] <- TRUE
    } else if (one_count > size/2) {
      # Trop de 1, marquer les 1 comme erreurs
      errors[line == 1] <- TRUE
    }
    
    return(errors)
  }
  
  # Fonction pour résoudre le Takuzu avec un algorithme de backtracking
  solveTakuzu <- function(grid) {
    # Fonction interne récursive
    solve_internal <- function(g, pos) {
      if (pos > size * size) {
        # Vérifier les lignes et colonnes pour la règle d'unicité
        for (i in 1:(size-1)) {
          for (j in (i+1):size) {
            if (all(g[i, ] == g[j, ]) || all(g[, i] == g[, j])) {
              return(NULL)  # Lignes ou colonnes identiques trouvées
            }
          }
        }
        return(g)  # Solution trouvée
      }
      
      # Calculer la position actuelle
      row <- ceiling(pos / size)
      col <- pos %% size
      if (col == 0) col <- size
      
      # Si la case est déjà remplie, passer à la suivante
      if (g[row, col] != -1) {
        return(solve_internal(g, pos + 1))
      }
      
      # Essayer les deux valeurs
      for (val in c(0, 1)) {
        # Vérifier si cette valeur est valide à cette position
        temp_g <- g
        temp_g[row, col] <- val
        
        # Vérifier les contraintes locales (pas plus de 2 consécutifs)
        if (col >= 3 && temp_g[row, col] == temp_g[row, col-1] && temp_g[row, col] == temp_g[row, col-2]) {
          next
        }
        if (row >= 3 && temp_g[row, col] == temp_g[row-1, col] && temp_g[row, col] == temp_g[row-2, col]) {
          next
        }
        
        # Vérifier que nous n'avons pas trop de 0 ou de 1 dans la ligne/colonne
        zeros_in_row <- sum(temp_g[row, ] == 0, na.rm = TRUE)
        ones_in_row <- sum(temp_g[row, ] == 1, na.rm = TRUE)
        zeros_in_col <- sum(temp_g[, col] == 0, na.rm = TRUE)
        ones_in_col <- sum(temp_g[, col] == 1, na.rm = TRUE)
        
        if (zeros_in_row > size/2 || ones_in_row > size/2 || zeros_in_col > size/2 || ones_in_col > size/2) {
          next
        }
        
        # Vérifier si la ligne est complète, elle doit être valide
        if (zeros_in_row + ones_in_row == size) {
          if (!isValidLine(temp_g[row, ])) {
            next
          }
        }
        
        # Vérifier si la colonne est complète, elle doit être valide
        if (zeros_in_col + ones_in_col == size) {
          if (!isValidLine(temp_g[, col])) {
            next
          }
        }
        
        # Récursivement essayer de résoudre avec cette valeur
        result <- solve_internal(temp_g, pos + 1)
        if (!is.null(result)) {
          return(result)
        }
      }
      
      # Aucune solution n'a été trouvée
      return(NULL)
    }
    
    # Commencer la résolution
    return(solve_internal(grid, 1))
  }
  
  # Initialiser le jeu
  initializeGame <- function() {
    # Générer une grille initiale en utilisant la fonction C++ existante
    new_initial <- generateTakuzu(size, fillRate)
    initial_grid(new_initial)
    
    # Créer une copie pour le joueur
    takuzu_grid(new_initial)
    
    # Résoudre la grille
    solution <- solveTakuzu(new_initial)
    
    # Si pas de solution, régénérer une nouvelle grille
    if (is.null(solution)) {
      initializeGame()
      return()
    }
    
    solution_grid(solution)
    hint_cells(matrix(FALSE, nrow = size, ncol = size))
    valid_rows(rep(FALSE, size))
    valid_cols(rep(FALSE, size))
    error_cells(matrix(FALSE, nrow = size, ncol = size))  # Réinitialiser les erreurs
    
    output$result <- renderText("Nouvelle partie commencée !")
  }
  
  # Initialiser le jeu au démarrage
  observe({
    if (is.null(takuzu_grid())) {
      initializeGame()
    }
  })
  
  # ---- Sélecteur dynamique de numéro de ligne/colonne ----
  output$line_number_selector <- renderUI({
    if (input$check_type == "grid") {
      return(NULL)  # Pas de sélecteur si on vérifie la grille entière
    } else {
      selectInput("line_number", "Numéro:", choices = 1:size)
    }
  })
  
  # ---- Affichage de la grille ----
  output$takuzu_ui <- renderUI({
    grid <- takuzu_grid()
    if (is.null(grid)) return(NULL)
    
    hints <- hint_cells()
    errors <- error_cells()
    
    cells <- lapply(1:size, function(row) {
      lapply(1:size, function(col) {
        cell_value <- grid[row, col]
        inputId <- paste0("cell_", row, "_", col)
        is_hint <- hints[row, col]
        is_initial <- initial_grid()[row, col] != -1
        is_error <- errors[row, col]
        
        cellClass <- "takuzu-btn"
        valueClass <- ifelse(cell_value == 0, "zero-cell", ifelse(cell_value == 1, "one-cell", ""))
        
        if (is_hint) {
          cellClass <- paste(cellClass, "hint-cell")
        }
        
        if (is_error) {
          cellClass <- paste(cellClass, "incorrect-cell")
        }
        
        if (cell_value != -1) {
          cellClass <- paste(cellClass, valueClass)
        }
        
        cell_ui <- div(class = "cell-wrapper",
                       actionButton(
                         inputId = inputId,
                         label = ifelse(cell_value == -1, " ", as.character(cell_value)),
                         class = cellClass,
                         disabled = is_initial
                       )
        )
        
        cell_ui
      })
    })
    
    # Affichage sous forme de tableau HTML
    do.call(fluidRow, lapply(cells, function(row) {
      column(12, do.call(splitLayout, c(row, list(cellWidths = rep("42px", size)))))
    }))
  })
  
  # ---- Gestion des clics de l'utilisateur ----
  for (row in 1:size) {
    for (col in 1:size) {
      local({
        r <- row
        c <- col
        input_id <- paste0("cell_", r, "_", c)
        
        observeEvent(input[[input_id]], {
          # S'assurer que les valeurs réactives sont bien initialisées
          if (is.null(takuzu_grid()) || is.null(initial_grid())) return()
          
          grid <- takuzu_grid()
          init_grid <- initial_grid()
          errors <- error_cells()
          
          # Seules les cases vides doivent être modifiables
          if (init_grid[r, c] == -1) {
            if (grid[r, c] == -1) {
              grid[r, c] <- 0  # Premier clic donne 0
            } else if (grid[r, c] == 0) {
              grid[r, c] <- 1  # Second clic donne 1
            } else {
              grid[r, c] <- -1  # Troisième clic vide la case
            }
            takuzu_grid(grid)
            
            # Effacer les erreurs lors du changement
            errors[r, c] <- FALSE
            error_cells(errors)
            
            # Mise à jour des statuts de validité sans affichage de checkmarks
            # MODIFIER: Ne pas appeler check_row_valid et check_col_valid ici
            # On stocke juste l'état de validité sans colorer les erreurs
            update_row_status(r)
            update_col_status(c)
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }
  }
  
  # NOUVEAU: Fonction pour mettre à jour le statut d'une ligne sans marquer les erreurs
  update_row_status <- function(row_idx) {
    grid <- takuzu_grid()
    # Vérifier si la grille existe
    if (is.null(grid)) return(FALSE)
    
    row_data <- grid[row_idx, ]
    
    # Vérifier si la ligne est valide
    is_valid <- !any(row_data == -1) && 
      sum(row_data == 0) == size/2 && 
      sum(row_data == 1) == size/2 &&
      !any(rle(row_data)$lengths > 2)
    
    # Mettre à jour le statut de validité
    v_rows <- valid_rows()
    if (is.null(v_rows)) return(FALSE)
    
    v_rows[row_idx] <- is_valid
    valid_rows(v_rows)
    
    # Vérifier si toutes les lignes et colonnes sont valides
    check_game_complete()
    
    return(is_valid)
  }
  
  # NOUVEAU: Fonction pour mettre à jour le statut d'une colonne sans marquer les erreurs
  update_col_status <- function(col_idx) {
    grid <- takuzu_grid()
    # Vérifier si la grille existe
    if (is.null(grid)) return(FALSE)
    
    col_data <- grid[, col_idx]
    
    # Vérifier si la colonne est valide
    is_valid <- !any(col_data == -1) && 
      sum(col_data == 0) == size/2 && 
      sum(col_data == 1) == size/2 &&
      !any(rle(col_data)$lengths > 2)
    
    # Mettre à jour le statut de validité
    v_cols <- valid_cols()
    if (is.null(v_cols)) return(FALSE)
    
    v_cols[col_idx] <- is_valid
    valid_cols(v_cols)
    
    # Vérifier si toutes les lignes et colonnes sont valides
    check_game_complete()
    
    return(is_valid)
  }
  
  # MODIFIÉ: Fonction pour vérifier une ligne et mettre en évidence les erreurs
  check_row_valid <- function(row_idx) {
    grid <- takuzu_grid()
    # Vérifier si la grille existe
    if (is.null(grid)) return(FALSE)
    
    row_data <- grid[row_idx, ]
    
    # Vérifier s'il y a des cases vides
    if (any(row_data == -1)) {
      # Ligne incomplète, on ne marque pas les erreurs
      is_valid <- FALSE
    } else {
      # Vérifier la validité
      is_valid <- sum(row_data == 0) == size/2 && 
        sum(row_data == 1) == size/2 &&
        !any(rle(row_data)$lengths > 2)
      
      # Si la ligne est invalide, marquer les cases en erreur
      if (!is_valid) {
        errors <- error_cells()
        row_errors <- findErrorsInLine(row_data)
        if (!is.null(row_errors)) {
          for (col in 1:size) {
            errors[row_idx, col] <- row_errors[col]
          }
          error_cells(errors)
        }
      }
    }
    
    # Mettre à jour le statut de validité
    v_rows <- valid_rows()
    if (is.null(v_rows)) return(FALSE)
    
    v_rows[row_idx] <- is_valid
    valid_rows(v_rows)
    
    # Vérifier si toutes les lignes et colonnes sont valides
    check_game_complete()
    
    return(is_valid)
  }
  
  # MODIFIÉ: Fonction pour vérifier une colonne et mettre en évidence les erreurs
  check_col_valid <- function(col_idx) {
    grid <- takuzu_grid()
    # Vérifier si la grille existe
    if (is.null(grid)) return(FALSE)
    
    col_data <- grid[, col_idx]
    
    # Vérifier s'il y a des cases vides
    if (any(col_data == -1)) {
      # Colonne incomplète, on ne marque pas les erreurs
      is_valid <- FALSE
    } else {
      # Vérifier la validité
      is_valid <- sum(col_data == 0) == size/2 && 
        sum(col_data == 1) == size/2 &&
        !any(rle(col_data)$lengths > 2)
      
      # Si la colonne est invalide, marquer les cases en erreur
      if (!is_valid) {
        errors <- error_cells()
        col_errors <- findErrorsInLine(col_data)
        if (!is.null(col_errors)) {
          for (row in 1:size) {
            errors[row, col_idx] <- col_errors[row]
          }
          error_cells(errors)
        }
      }
    }
    
    # Mettre à jour le statut de validité
    v_cols <- valid_cols()
    if (is.null(v_cols)) return(FALSE)
    
    v_cols[col_idx] <- is_valid
    valid_cols(v_cols)
    
    # Vérifier si toutes les lignes et colonnes sont valides
    check_game_complete()
    
    return(is_valid)
  }
  
  # MODIFIÉ: Fonction pour vérifier la grille entière
  check_grid_valid <- function() {
    # Vérifier les lignes et colonnes
    all_rows_valid <- TRUE
    all_cols_valid <- TRUE
    grid_complete <- TRUE
    
    # Vérifier si la grille est complète
    if (any(takuzu_grid() == -1)) {
      grid_complete <- FALSE
    }
    
    # Réinitialiser les erreurs
    error_cells(matrix(FALSE, nrow = size, ncol = size))
    
    # Vérifier toutes les lignes
    for (i in 1:size) {
      if (!check_row_valid(i)) {
        all_rows_valid <- FALSE
      }
    }
    
    # Vérifier toutes les colonnes
    for (i in 1:size) {
      if (!check_col_valid(i)) {
        all_cols_valid <- FALSE
      }
    }
    
    # Vérifier les lignes et colonnes identiques
    duplicate_lines <- FALSE
    if (grid_complete && all_rows_valid && all_cols_valid) {
      grid <- takuzu_grid()
      
      # Vérifier les lignes identiques
      for (i in 1:(size-1)) {
        for (j in (i+1):size) {
          if (all(grid[i, ] == grid[j, ])) {
            # Marquer les lignes identiques comme erreurs
            errors <- error_cells()
            errors[i, ] <- TRUE
            errors[j, ] <- TRUE
            error_cells(errors)
            duplicate_lines <- TRUE
          }
        }
      }
      
      # Vérifier les colonnes identiques
      for (i in 1:(size-1)) {
        for (j in (i+1):size) {
          if (all(grid[, i] == grid[, j])) {
            # Marquer les colonnes identiques comme erreurs
            errors <- error_cells()
            errors[, i] <- TRUE
            errors[, j] <- TRUE
            error_cells(errors)
            duplicate_lines <- TRUE
          }
        }
      }
    }
    
    # Retourner le résultat
    if (!grid_complete) {
      return(list(valid = FALSE, message = "Grille incomplète"))
    } else if (!all_rows_valid) {
      return(list(valid = FALSE, message = "Certaines lignes sont incorrectes"))
    } else if (!all_cols_valid) {
      return(list(valid = FALSE, message = "Certaines colonnes sont incorrectes"))
    } else if (duplicate_lines) {
      return(list(valid = FALSE, message = "Grille invalide: lignes ou colonnes identiques détectées"))
    } else {
      return(list(valid = TRUE, message = "Grille valide!"))
    }
  }
  
  # Vérifier si le jeu est complet
  check_game_complete <- function() {
    # Vérifier si toutes les valeurs réactives sont initialisées
    if (is.null(valid_rows()) || is.null(valid_cols()) || is.null(takuzu_grid())) return()
    
    # Si toutes les lignes et colonnes sont valides
    if (all(valid_rows()) && all(valid_cols())) {
      # Vérifier la règle 3: pas de lignes/colonnes identiques
      grid <- takuzu_grid()
      
      # Vérifier les lignes identiques
      duplicate_rows <- FALSE
      for (i in 1:(size-1)) {
        for (j in (i+1):size) {
          if (all(grid[i, ] == grid[j, ])) {
            duplicate_rows <- TRUE
            break
          }
        }
        if (duplicate_rows) break
      }
      
      # Vérifier les colonnes identiques
      duplicate_cols <- FALSE
      for (i in 1:(size-1)) {
        for (j in (i+1):size) {
          if (all(grid[, i] == grid[, j])) {
            duplicate_cols <- TRUE
            break
          }
        }
        if (duplicate_cols) break
      }
      
      if (!duplicate_rows && !duplicate_cols) {
        output$result <- renderText("🎉 Bravo ! Grille complète et correcte !")
      }
      # Ne pas afficher de message automatique sur les lignes/colonnes identiques
      # Seulement lorsque l'utilisateur clique sur vérifier
    }
  }
  
  # ---- Vérification d'une ligne, colonne, ou de la grille entière ----
  observeEvent(input$check_line, {
    grid <- takuzu_grid()
    # Vérifier si la grille existe
    if (is.null(grid)) return()
    
    line_type <- input$check_type
    
    # Effacer toutes les erreurs actuelles
    error_cells(matrix(FALSE, nrow = size, ncol = size))
    
    # Vérifier la ligne, colonne, ou grille selon le type sélectionné
    if (line_type == "row") {
      line_num <- as.numeric(input$line_number)
      is_valid <- check_row_valid(line_num)
      if (is_valid) {
        output$result <- renderText(paste0("✅ Ligne ", line_num, " correcte!"))
      } else {
        output$result <- renderText(paste0("❌ Ligne ", line_num, " incorrecte! (cases erronées en rouge)"))
      }
    } else if (line_type == "col") {
      line_num <- as.numeric(input$line_number)
      is_valid <- check_col_valid(line_num)
      if (is_valid) {
        output$result <- renderText(paste0("✅ Colonne ", line_num, " correcte!"))
      } else {
        output$result <- renderText(paste0("❌ Colonne ", line_num, " incorrecte! (cases erronées en rouge)"))
      }
    } else if (line_type == "grid") {
      # Vérifier la grille entière
      result <- check_grid_valid()
      if (result$valid) {
        output$result <- renderText(paste0("🎉 ", result$message))
      } else {
        output$result <- renderText(paste0("❌ ", result$message, " (cases erronées en rouge)"))
      }
    }
  })
  
  # ---- Obtenir un indice ----
  observeEvent(input$get_hint, {
    grid <- takuzu_grid()
    solution <- solution_grid()
    hints <- hint_cells()
    
    # Vérifier si toutes les valeurs nécessaires existent
    if (is.null(grid) || is.null(solution) || is.null(hints) || is.null(initial_grid())) return()
    
    # Effacer toutes les erreurs actuelles lors de l'obtention d'un indice
    error_cells(matrix(FALSE, nrow = size, ncol = size))
    
    # Trouver les cases vides ou incorrectes
    candidates <- c()
    for (i in 1:size) {
      for (j in 1:size) {
        if (grid[i, j] == -1 || (grid[i, j] != solution[i, j] && !hints[i, j] && initial_grid()[i, j] == -1)) {
          candidates <- c(candidates, c(i, j))
        }
      }
    }
    
    if (length(candidates) >= 2) {
      # Sélectionner une position au hasard
      pos_idx <- sample(1:(length(candidates)/2), 1)
      row_idx <- candidates[2*pos_idx-1]
      col_idx <- candidates[2*pos_idx]
      
      # Mettre à jour la grille avec la valeur correcte
      grid[row_idx, col_idx] <- solution[row_idx, col_idx]
      takuzu_grid(grid)
      
      # Marquer cette case comme indice
      hints[row_idx, col_idx] <- TRUE
      hint_cells(hints)
      
      # Mettre à jour les statuts sans marquer les erreurs
      update_row_status(row_idx)
      update_col_status(col_idx)
      
      output$result <- renderText(paste0("💡 Indice placé en position (", row_idx, ",", col_idx, ")"))
    } else {
      output$result <- renderText("Aucun indice supplémentaire disponible")
    }
  })
  
  # ---- Afficher la solution complète ----
  observeEvent(input$show_solution, {
    solution <- solution_grid()
    # Vérifier si la solution existe
    if (is.null(solution)) return()
    
    # Mettre à jour toutes les cases avec la solution
    takuzu_grid(solution)
    
    # Effacer les erreurs
    error_cells(matrix(FALSE, nrow = size, ncol = size))
    
    # Marquer toutes les lignes et colonnes comme valides
    valid_rows(rep(TRUE, size))
    valid_cols(rep(TRUE, size))
    
    output$result <- renderText("Solution complète affichée")
  })
  
  # ---- Générer une nouvelle grille ----
  observeEvent(input$new_game, {
    initializeGame()
  })
}

# ---- Lancer l'application ----
shinyApp(ui, server)