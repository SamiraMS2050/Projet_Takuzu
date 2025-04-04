library(Rcpp)
sourceCpp("takuzu.cpp")
library(shiny)
library(shinyjs)

# ---- Interface Utilisateur (UI) ----
ui <- fluidPage(
useShinyjs(),
  tags$head(
    tags$style(HTML("
    #game_page {
background: linear-gradient(135deg, #6e8efb, #a777e3); /* fond vert dégradé */
color: black; /* texte blanc */
padding: 20px;
min-height: 100vh;
}

.sidebarPanel {
background-color: rgba(255, 255, 255, 0.15);
border-radius: 10px;
padding: 15px;
color: white;
}

.mainPanel {
background-color: rgba(255, 255, 255, 0.05);
padding: 15px;
border-radius: 10px;
}

h4 {
color: #000000;
}
h3, h1 {
color: #ffffff;
}

.btn-primary, .btn-warning, .btn-danger, .btn-info, .btn-default {
border: none;
border-radius: 8px;
font-weight: bold;
}

.btn-primary {
background-color: #6e8efb !important;
}

.btn-danger {
background-color: #6e8efb !important;
}

.btn-warning {
background-color: #6e8efb !important;
}

.btn-info {
background-color: #6e8efb !important;
}

.back_menu {
background-color: #6a1b9a !important;
}

      #welcome_page {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 100vh;
        text-align: center;
        background: linear-gradient(135deg, #6e8efb, #a777e3);
        color: white;
        padding: 20px;
      }
      .rules-panel {
        background: rgba(255,255,255,0.2);
        border-radius: 10px;
        padding: 20px;
        margin: 20px 0;
        max-width: 700px;
      }
      .rules-panel ul li {
  font-size: 1.2em;  /* Taille augmentée pour les items */
  color: white;
}
      .start-btn {
        font-size: 1.2em;
        padding: 10px 30px;
        margin-top: 20px;
        background-color: #6e8efb;
      }
      .difficulty-btn {
        font-size: 1em;
        padding: 8px 20px;
        margin: 5px;
        width: 150px;
      }
      .selected-difficulty {
        background-color: #6e8efb !important;
        color: white !important;
      }
      .takuzu-title {
  color: white;
  font-size: 1em;
  font-weight: bold;
    text-align: center;
      }
    .takuzu {
  color: white;
  font-size: 4em;
  font-weight: bold;
    text-align: center;
}
      .takuzu-btn {
        width: 50px !important;
        height: 50px !important;
        margin: 1px !important;
        font-weight: bold;
        font-size: 20px;
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
  
  div(
    id = "welcome_page",
    h1("Jeu Takuzu",class="takuzu"),
    div(
      class = "rules-panel",
      h3("Règles du jeu :"),
      tags$ul(
        tags$li("Chaque case doit contenir un 0 ou un 1"),
        tags$li("Chaque ligne et colonne doit avoir autant de 0 que de 1"),
        tags$li("Pas plus de deux 0 ou deux 1 côte à côte"),
        tags$li("Deux lignes ou deux colonnes identiques sont interdites")
      ),
      h3("Choisissez votre niveau de difficulté :"),
      tags$ul(
        tags$li("Facile:Grille de taille (6×6) remplie à 40%"),
        tags$li("Moyen:Grille de taille (8×8) remplie à 30%"),
        tags$li("Difficile:Grille de taille (8×8) remplie à 10%"),
      ),
      actionButton("easy", "Facile", class = "difficulty-btn"),
      actionButton("medium", "Moyen ", class = "difficulty-btn"),
      actionButton("hard", "Difficile ", class = "difficulty-btn")
    ),
    actionButton("start_game", "Commencer", class = "start-btn btn-primary")
  ),
  
  hidden(
    div(
      id = "game_page",
      titlePanel(div(class = "takuzu-title", "Jeu Takuzu")),
      sidebarLayout(
        sidebarPanel(
          actionButton("back_to_menu", "Retour au menu", class = "btn-primary btn-block"),
          actionButton("new_game", "Nouvelle Grille", class = "btn-primary btn-block"),
          hr(),
          wellPanel(
            h4("Vérification"),
            selectInput("check_type", "Type:", 
                        choices = c("Ligne" = "row", "Colonne" = "col", "Grille" = "grid")),
            uiOutput("line_number_selector"),
            actionButton("check_line", "Vérification", class = "btn-info btn-block")
          ),
          actionButton("get_hint", "Indice", class = "btn-warning btn-block"),
          hr(),
          actionButton("show_solution", "Solution de la grille", class = "btn-danger btn-block"),
          hr(),
          textOutput("result")
        ),
        mainPanel(
           uiOutput("takuzu_ui")
        )
      )
    )
  )
)

# ---- Serveur ----
server <- function(input, output, session) {
  # Variable réactive pour la taille de la grille
  size <- reactiveVal(8)  # Taille par défaut (pour niveau moyen)
  difficulty <- reactiveVal("medium")
  
  # Grilles réactives
  initial_grid <- reactiveVal(NULL)
  takuzu_grid <- reactiveVal(NULL)
  solution_grid <- reactiveVal(NULL)
  hint_cells <- reactiveVal(NULL)
  valid_rows <- reactiveVal(NULL)
  valid_cols <- reactiveVal(NULL)
  error_cells <- reactiveVal(NULL)
  
  # Gestion de la sélection de difficulté
  observeEvent(input$easy, {
    difficulty("easy")
    size(6)  # Grille 6x6 pour niveau facile
    shinyjs::removeClass("medium", "selected-difficulty")
    shinyjs::removeClass("hard", "selected-difficulty")
    shinyjs::addClass("easy", "selected-difficulty")
  })
  
  observeEvent(input$medium, {
    difficulty("medium")
    size(8)  # Grille 8x8 pour niveau moyen
    shinyjs::removeClass("easy", "selected-difficulty")
    shinyjs::removeClass("hard", "selected-difficulty")
    shinyjs::addClass("medium", "selected-difficulty")
  })
  
  observeEvent(input$hard, {
    difficulty("hard")
    size(8)  # Grille 10x10 pour niveau difficile
    shinyjs::removeClass("easy", "selected-difficulty")
    shinyjs::removeClass("medium", "selected-difficulty")
    shinyjs::addClass("hard", "selected-difficulty")
  })
  
  # Passage à l'interface de jeu
  observeEvent(input$start_game, {
    shinyjs::hide("welcome_page")
    shinyjs::show("game_page")
    initializeGame()
  })
  
  # Gestion du retour au menu
  observeEvent(input$back_to_menu, {
    shinyjs::hide("game_page")
    shinyjs::show("welcome_page")
  })
  
  # Fonction pour obtenir le taux de remplissage
  getFillRate <- function() {
    switch(difficulty(),
           "easy"=0.4,
          "medium"=0.3,
          "hard"=0.1) # Taux de remplissage fixé à 40% pour tous les niveaux
  }
  
  # Fonction pour vérifier si une valeur est valide
  isValidPlacement <- function(grid, row, col, val) {
    current_size <- size()
    
    if (col > 2 && grid[row, col-1] == val && grid[row, col-2] == val) return(FALSE)
    if (row > 2 && grid[row-1, col] == val && grid[row-2, col] == val) return(FALSE)
    
    zero_count_row <- sum(grid[row, ] == 0, na.rm = TRUE)
    zero_count_col <- sum(grid[, col] == 0, na.rm = TRUE)
    
    if (val == 0 && (zero_count_row >= current_size/2 || zero_count_col >= current_size/2)) return(FALSE)
    
    return(TRUE)
  }
  
  # Fonction pour résoudre le Takuzu
  solveTakuzu <- function(grid) {
    current_size <- size()
    
    solve_internal <- function(g, pos) {
      if (pos > current_size * current_size) {
        for (i in 1:(current_size-1)) {
          for (j in (i+1):current_size) {
            if (all(g[i, ] == g[j, ]) || all(g[, i] == g[, j])) return(NULL)
          }
        }
        return(g)
      }
      
      row <- ceiling(pos / current_size)
      col <- pos %% current_size
      if (col == 0) col <- current_size
      
      if (g[row, col] != -1) return(solve_internal(g, pos + 1))
      
      for (val in c(0, 1)) {
        temp_g <- g
        temp_g[row, col] <- val
        
        if (col >= 3 && temp_g[row, col] == temp_g[row, col-1] && temp_g[row, col] == temp_g[row, col-2]) next
        if (row >= 3 && temp_g[row, col] == temp_g[row-1, col] && temp_g[row, col] == temp_g[row-2, col]) next
        
        zeros_in_row <- sum(temp_g[row, ] == 0, na.rm = TRUE)
        ones_in_row <- sum(temp_g[row, ] == 1, na.rm = TRUE)
        zeros_in_col <- sum(temp_g[, col] == 0, na.rm = TRUE)
        ones_in_col <- sum(temp_g[, col] == 1, na.rm = TRUE)
        
        if (zeros_in_row > current_size/2 || ones_in_row > current_size/2 || zeros_in_col > current_size/2 || ones_in_col > current_size/2) next
        
        if (zeros_in_row + ones_in_row == current_size && !isValidLine(temp_g[row, ])) next
        if (zeros_in_col + ones_in_col == current_size && !isValidLine(temp_g[, col])) next
        
        result <- solve_internal(temp_g, pos + 1)
        if (!is.null(result)) return(result)
      }
      return(NULL)
    }
    return(solve_internal(grid, 1))
  }
  
  isValidLine <- function(line) {
    current_size <- size()
    if (sum(line == 0) != sum(line == 1)) return(FALSE)
    if (any(rle(line)$lengths > 2)) return(FALSE)
    return(TRUE)
  }
  
  findErrorsInLine <- function(line) {
    errors <- rep(FALSE, length(line))
    for (i in 2:(length(line)-1)) {
      if (line[i-1] == line[i] && line[i] == line[i+1]) {
        errors[(i-1):(i+1)] <- TRUE
      }
    }
    return(errors)
  }
  
  # Initialiser le jeu
  initializeGame <- function() {
    current_size <- size()
    new_initial <- generateTakuzu(current_size, getFillRate())
    initial_grid(new_initial)
    takuzu_grid(new_initial)
    
    solution <- solveTakuzu(new_initial)
    if (is.null(solution)) {
      initializeGame()
      return()
    }
    
    solution_grid(solution)
    hint_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    valid_rows(rep(FALSE, current_size))
    valid_cols(rep(FALSE, current_size))
    error_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    
    # Réinitialiser les observateurs pour les cellules
    setupCellObservers()
    
    output$result <- renderText(paste("Nouvelle partie - Niveau:", difficulty()))
  }
  
  observe({ if (is.null(takuzu_grid())) initializeGame() })
  
  output$line_number_selector <- renderUI({
    if (input$check_type == "grid") return(NULL)
    selectInput("line_number", "Numéro:", choices = 1:size())
  })
  
  output$takuzu_ui <- renderUI({
    grid <- takuzu_grid()
    if (is.null(grid)) return(NULL)
    
    current_size <- size()
    hints <- hint_cells()
    errors <- error_cells()
    
    cells <- lapply(1:current_size, function(row) {
      lapply(1:current_size, function(col) {
        cell_value <- grid[row, col]
        inputId <- paste0("cell_", row, "_", col)
        is_hint <- hints[row, col]
        is_initial <- initial_grid()[row, col] != -1
        is_error <- errors[row, col]
        
        cellClass <- "takuzu-btn"
        if (is_hint) cellClass <- paste(cellClass, "hint-cell")
        if (is_error) cellClass <- paste(cellClass, "incorrect-cell")
        if (cell_value != -1) cellClass <- paste(cellClass, ifelse(cell_value == 0, "zero-cell", "one-cell"))
        
        div(class = "cell-wrapper",
            actionButton(
              inputId = inputId,
              label = ifelse(cell_value == -1, " ", as.character(cell_value)),
              class = cellClass,
              disabled = is_initial
            )
        )
      })
    })
    
    do.call(fluidRow, lapply(cells, function(row) {
      column(12, do.call(splitLayout, c(row, list(cellWidths = rep("60px", current_size)))))
    }))
  })
  
  # Correction pour la gestion des cellules 10x10
  setupCellObservers <- function() {
    current_size <- size()
    
    # Supprimer tous les observateurs existants pour éviter les doublons
    for (row in 1:20) {  # Utilisation d'un nombre suffisamment grand
      for (col in 1:20) {
        if (exists(paste0("cell_", row, "_", col, "_observer"))) {
          try(silent = TRUE, {
            observeEvent(input[[paste0("cell_", row, "_", col)]], {}, 
                         ignoreNULL = TRUE, ignoreInit = TRUE, suspended = TRUE)
          })
        }
      }
    }
    
    # Créer de nouveaux observateurs pour chaque cellule
    for (row in 1:current_size) {
      for (col in 1:current_size) {
        local({
          local_row <- row
          local_col <- col
          input_id <- paste0("cell_", local_row, "_", local_col)
          
          assign(paste0(input_id, "_observer"), 
                 observeEvent(input[[input_id]], {
                   if (is.null(takuzu_grid()) || is.null(initial_grid())) return()
                   
                   grid <- takuzu_grid()
                   init_grid <- initial_grid()
                   errors <- error_cells()
                   
                   if (init_grid[local_row, local_col] == -1) {
                     if (grid[local_row, local_col] == -1) grid[local_row, local_col] <- 0
                     else if (grid[local_row, local_col] == 0) grid[local_row, local_col] <- 1
                     else grid[local_row, local_col] <- -1
                     
                     takuzu_grid(grid)
                     errors[local_row, local_col] <- FALSE
                     error_cells(errors)
                     update_row_status(local_row)
                     update_col_status(local_col)
                   }
                 }, ignoreNULL = TRUE, ignoreInit = TRUE), 
                 envir = parent.frame())
        })
      }
    }
  }
  
  update_row_status <- function(row_idx) {
    grid <- takuzu_grid()
    if (is.null(grid)) return(FALSE)
    
    current_size <- size()
    row_data <- grid[row_idx, ]
    is_valid <- !any(row_data == -1) && sum(row_data == 0) == current_size/2 && 
      sum(row_data == 1) == current_size/2 && !any(rle(row_data)$lengths > 2)
    
    v_rows <- valid_rows()
    v_rows[row_idx] <- is_valid
    valid_rows(v_rows)
    check_game_complete()
    return(is_valid)
  }
  
  update_col_status <- function(col_idx) {
    grid <- takuzu_grid()
    if (is.null(grid)) return(FALSE)
    
    current_size <- size()
    col_data <- grid[, col_idx]
    is_valid <- !any(col_data == -1) && sum(col_data == 0) == current_size/2 && 
      sum(col_data == 1) == current_size/2 && !any(rle(col_data)$lengths > 2)
    
    v_cols <- valid_cols()
    v_cols[col_idx] <- is_valid
    valid_cols(v_cols)
    check_game_complete()
    return(is_valid)
  }
  
  check_row_valid <- function(row_idx) {
    grid <- takuzu_grid()
    if (is.null(grid)) return(FALSE)
    
    current_size <- size()
    row_data <- grid[row_idx, ]
    is_valid <- !any(row_data == -1) && sum(row_data == 0) == current_size/2 && 
      sum(row_data == 1) == current_size/2 && !any(rle(row_data)$lengths > 2)
    
    if (!is_valid && !any(row_data == -1)) {
      errors <- error_cells()
      row_errors <- findErrorsInLine(row_data)
      for (col in 1:current_size) errors[row_idx, col] <- row_errors[col]
      error_cells(errors)
    }
    
    v_rows <- valid_rows()
    v_rows[row_idx] <- is_valid
    valid_rows(v_rows)
    check_game_complete()
    return(is_valid)
  }
  
  check_col_valid <- function(col_idx) {
    grid <- takuzu_grid()
    if (is.null(grid)) return(FALSE)
    
    current_size <- size()
    col_data <- grid[, col_idx]
    is_valid <- !any(col_data == -1) && sum(col_data == 0) == current_size/2 && 
      sum(col_data == 1) == current_size/2 && !any(rle(col_data)$lengths > 2)
    
    if (!is_valid && !any(col_data == -1)) {
      errors <- error_cells()
      col_errors <- findErrorsInLine(col_data)
      for (row in 1:current_size) errors[row, col_idx] <- col_errors[row]
      error_cells(errors)
    }
    
    v_cols <- valid_cols()
    v_cols[col_idx] <- is_valid
    valid_cols(v_cols)
    check_game_complete()
    return(is_valid)
  }
  
  check_grid_valid <- function() {
    current_size <- size()
    all_rows_valid <- TRUE
    all_cols_valid <- TRUE
    grid_complete <- !any(takuzu_grid() == -1)
    
    error_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    
    for (i in 1:current_size) if (!check_row_valid(i)) all_rows_valid <- FALSE
    for (i in 1:current_size) if (!check_col_valid(i)) all_cols_valid <- FALSE
    
    duplicate_lines <- FALSE
    if (grid_complete && all_rows_valid && all_cols_valid) {
      grid <- takuzu_grid()
      errors <- error_cells()
      
      for (i in 1:(current_size-1)) {
        for (j in (i+1):current_size) {
          if (all(grid[i, ] == grid[j, ])) {
            errors[i, ] <- TRUE
            errors[j, ] <- TRUE
            duplicate_lines <- TRUE
          }
          if (all(grid[, i] == grid[, j])) {
            errors[, i] <- TRUE
            errors[, j] <- TRUE
            duplicate_lines <- TRUE
          }
        }
      }
      error_cells(errors)
    }
    
    if (!grid_complete) return(list(valid = FALSE, message = "Grille incomplète"))
    if (!all_rows_valid) return(list(valid = FALSE, message = "Certaines lignes sont incorrectes"))
    if (!all_cols_valid) return(list(valid = FALSE, message = "Certaines colonnes sont incorrectes"))
    if (duplicate_lines) return(list(valid = FALSE, message = "Lignes ou colonnes identiques détectées"))
    return(list(valid = TRUE, message = "Grille valide!"))
  }
  
  check_game_complete <- function() {
    if (is.null(valid_rows()) || is.null(valid_cols()) || is.null(takuzu_grid())) return()
    
    current_size <- size()
    if (all(valid_rows()) && all(valid_cols())) {
      grid <- takuzu_grid()
      duplicate <- FALSE
      
      for (i in 1:(current_size-1)) {
        for (j in (i+1):current_size) {
          if (all(grid[i, ] == grid[j, ]) || all(grid[, i] == grid[, j])) {
            duplicate <- TRUE
            break
          }
        }
        if (duplicate) break
      }
      
      if (!duplicate) output$result <- renderText("🎉 Bravo ! Grille complète et correcte !")
    }
  }
  
  observeEvent(input$check_line, {
    grid <- takuzu_grid()
    if (is.null(grid)) return()
    
    current_size <- size()
    line_type <- input$check_type
    error_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    
    if (line_type == "row") {
      line_num <- as.numeric(input$line_number)
      is_valid <- check_row_valid(line_num)
      output$result <- renderText(ifelse(is_valid, 
                                         paste0("✅ Ligne ", line_num, " correcte!"),
                                         paste0("❌ Ligne ", line_num, " incorrecte!")))
    } else if (line_type == "col") {
      line_num <- as.numeric(input$line_number)
      is_valid <- check_col_valid(line_num)
      output$result <- renderText(ifelse(is_valid,
                                         paste0("✅ Colonne ", line_num, " correcte!"),
                                         paste0("❌ Colonne ", line_num, " incorrecte!")))
    } else {
      result <- check_grid_valid()
      output$result <- renderText(paste0(ifelse(result$valid, "🎉 ", "❌ "), result$message))
    }
  })
  
  observeEvent(input$get_hint, {
    grid <- takuzu_grid()
    solution <- solution_grid()
    hints <- hint_cells()
    
    if (is.null(grid) || is.null(solution) || is.null(hints) || is.null(initial_grid())) return()
    
    current_size <- size()
    error_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    candidates <- c()
    
    for (i in 1:current_size) {
      for (j in 1:current_size) {
        if (grid[i, j] == -1 || (grid[i, j] != solution[i, j] && !hints[i, j] && initial_grid()[i, j] == -1)) {
          candidates <- c(candidates, c(i, j))
        }
      }
    }
    
    if (length(candidates) >= 2) {
      pos_idx <- sample(1:(length(candidates)/2), 1)
      row_idx <- candidates[2*pos_idx-1]
      col_idx <- candidates[2*pos_idx]
      
      grid[row_idx, col_idx] <- solution[row_idx, col_idx]
      takuzu_grid(grid)
      
      hints[row_idx, col_idx] <- TRUE
      hint_cells(hints)
      
      update_row_status(row_idx)
      update_col_status(col_idx)
      
      output$result <- renderText(paste0("💡 Indice en (", row_idx, ",", col_idx, ")"))
    } else {
      output$result <- renderText("Aucun indice disponible")
    }
  })
  
  observeEvent(input$show_solution, {
    solution <- solution_grid()
    if (is.null(solution)) return()
    
    takuzu_grid(solution)
    current_size <- size()
    error_cells(matrix(FALSE, nrow = current_size, ncol = current_size))
    valid_rows(rep(TRUE, current_size))
    valid_cols(rep(TRUE, current_size))
    output$result <- renderText("Solution affichée")
  })
  
  observeEvent(input$new_game, {
    initializeGame()
  })
}

shinyApp(ui, server)