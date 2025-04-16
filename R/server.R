#' Fonction serveur pour l'application Takuzu
#'
#' Cette fonction définit la logique côté serveur de l'application Shiny Takuzu.
#' Elle gère la génération des grilles, la validation des mouvements, et l'interaction avec l'utilisateur.
#'
#' @param input Objet input Shiny.
#' @param output Objet output Shiny.
#' @param session Session Shiny actuelle.
#'
#' @return Une fonction serveur Shiny.
#' @export
#'
#' @examples
#' \dontrun{
#' shinyApp(ui = takuzuUI(), server = takuzuServer)
#' }
takuzuServer <- function(input, output, session) {
  # Variables réactives pour la taille de la grille
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

  #' @keywords internal
  #' Gestion de la sélection de difficulté - niveau facile
  observeEvent(input$easy, {
    difficulty("easy")
    size(6)  # Grille 6x6 pour niveau facile
    shinyjs::removeClass("medium", "selected-difficulty")
    shinyjs::removeClass("hard", "selected-difficulty")
    shinyjs::addClass("easy", "selected-difficulty")
  })

  #' @keywords internal
  #' Gestion de la sélection de difficulté - niveau moyen
  observeEvent(input$medium, {
    difficulty("medium")
    size(8)  # Grille 8x8 pour niveau moyen
    shinyjs::removeClass("easy", "selected-difficulty")
    shinyjs::removeClass("hard", "selected-difficulty")
    shinyjs::addClass("medium", "selected-difficulty")
  })

  #' @keywords internal
  #' Gestion de la sélection de difficulté - niveau difficile
  observeEvent(input$hard, {
    difficulty("hard")
    size(8)
    shinyjs::removeClass("easy", "selected-difficulty")
    shinyjs::removeClass("medium", "selected-difficulty")
    shinyjs::addClass("hard", "selected-difficulty")
  })

  #' @keywords internal
  #' Passage à l'interface de jeu
  observeEvent(input$start_game, {
    shinyjs::hide("welcome_page")
    shinyjs::show("game_page")
    initializeGame()
  })

  #' @keywords internal
  #' Gestion du retour au menu
  observeEvent(input$back_to_menu, {
    shinyjs::hide("game_page")
    shinyjs::show("welcome_page")
  })
  #' Détermine le taux de remplissage initial de la grille selon la difficulté
  #'
  #' @return Le pourcentage de cellules pré-remplies
  #' @keywords internal
  getFillRate <- function() {
    switch(difficulty(),
           "easy"=0.4,
           "medium"=0.3,
           "hard"=0.1)
  }


  #' Vérifie si un placement est valide selon les règles du Takuzu
  #'
  #' Cette fonction vérifie si le placement d'une valeur (0 ou 1) à une position donnée
  #' respecte les règles du Takuzu :
  #' 1. Pas plus de deux chiffres identiques consécutifs
  #' 2. Pas plus de la moitié de la grille avec des 0 (ou des 1) sur une ligne ou colonne
  #'
  #' @param grid Matrice représentant la grille de jeu actuelle
  #' @param row Indice de la ligne où placer la valeur
  #' @param col Indice de la colonne où placer la valeur
  #' @param val Valeur à placer (0 ou 1)
  #' @return Booléen indiquant si le placement est valide
  #'
  #' @examples
  #' \dontrun{
  #' # Supposons une grille 6x6 partiellement remplie
  #' grid <- matrix(-1, nrow = 6, ncol = 6)
  #' grid[1, 1:3] <- c(0, 0, -1)
  #'
  #' # Vérifier si on peut placer un 0 en position (1,3)
  #' isValidPlacement(grid, 1, 3, 0)  # FALSE car cela créerait trois 0 consécutifs
  #'
  #' # Vérifier si on peut placer un 1 en position (1,3)
  #' isValidPlacement(grid, 1, 3, 1)  # TRUE
  #' }
  #'
  #' @keywords internal

  isValidPlacement <- function(grid, row, col, val) {
    current_size <- size()

    if (col > 2 && grid[row, col-1] == val && grid[row, col-2] == val) return(FALSE)
    if (row > 2 && grid[row-1, col] == val && grid[row-2, col] == val) return(FALSE)

    zero_count_row <- sum(grid[row, ] == 0, na.rm = TRUE)
    zero_count_col <- sum(grid[, col] == 0, na.rm = TRUE)

    if (val == 0 && (zero_count_row >= current_size/2 || zero_count_col >= current_size/2)) return(FALSE)

    return(TRUE)
  }

  #' Résout une grille de Takuzu
  #'
  #' @param grid Grille initiale à résoudre
  #' @return Une matrice représentant la solution, ou NULL si pas de solution
  #' @keywords internal

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

  #' Vérifie si une ligne ou colonne de Takuzu respecte les règles du jeu
  #'
  #' Cette fonction vérifie qu'une ligne ou colonne respecte les deux règles principales du Takuzu :
  #' 1. Un nombre égal de 0 et de 1
  #' 2. Pas plus de deux chiffres identiques consécutifs
  #'
  #' @param line Vecteur contenant les valeurs (0, 1) d'une ligne ou colonne
  #' @return Booléen indiquant si la ligne ou colonne est valide selon les règles
  #'
  #' @examples
  #' \dontrun{
  #' # Ligne valide avec 3 zeros et 3 uns, sans plus de 2 chiffres identiques consécutifs
  #' isValidLine(c(0, 1, 0, 1, 0, 1))  # TRUE
  #'
  #' # Ligne invalide car elle contient trois 1 consécutifs
  #' isValidLine(c(0, 0, 1, 1, 1, 0))  # FALSE
  #' }
  #'
  #' @keywords internal
  isValidLine <- function(line) {
    current_size <- size()
    if (sum(line == 0) != sum(line == 1)) return(FALSE)
    if (any(rle(line)$lengths > 2)) return(FALSE)
    return(TRUE)
  }
  #' Identifie les erreurs dans une ligne ou colonne
  #'
  #' @param line_data Vecteur représentant une ligne ou colonne
  #' @return Vecteur de booléens indiquant les cellules en erreur
  #' @keywords internal
  findErrorsInLine <- function(line) {
    errors <- rep(FALSE, length(line))
    for (i in 2:(length(line)-1)) {
      if (line[i-1] == line[i] && line[i] == line[i+1]) {
        errors[(i-1):(i+1)] <- TRUE
      }
    }
    return(errors)
  }

  #' @keywords internal
  #' Initialise une nouvelle partie de Takuzu
  #'
  #' Cette fonction génère une nouvelle grille de Takuzu selon la difficulté choisie,
  #' calcule sa solution et initialise les variables de jeu.
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

  #' @keywords internal
  #' Initialise le jeu si nécessaire
  observe({
    if (is.null(takuzu_grid())) initializeGame()
  })

  #' @keywords internal
  #' Interface pour la sélection de ligne/colonne à vérifier
  output$line_number_selector <- renderUI({
    if (input$check_type == "grid") return(NULL)
    selectInput("line_number", "Numéro:", choices = 1:size())
  })

  #' @keywords internal
  #' Rendu de l'interface de la grille Takuzu
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

  #' @keywords internal
  #' Configure les observateurs pour chaque cellule de la grille
  #'
  #' Cette fonction crée des observateurs dynamiques pour toutes les cellules
  #' de la grille en fonction de la taille actuelle.
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

  #' @keywords internal
  #' Met à jour le statut de validité d'une ligne
  #'
  #' @param row_idx Indice de la ligne à vérifier
  #' @return Booléen indiquant si la ligne est valide
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

  #' @keywords internal
  #' Met à jour le statut de validité d'une colonne
  #'
  #' @param col_idx Indice de la colonne à vérifier
  #' @return Booléen indiquant si la colonne est valide
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

  #' @keywords internal
  #' Vérifie la validité d'une ligne et identifie les erreurs éventuelles
  #'
  #' @param row_idx Indice de la ligne à vérifier
  #' @return Booléen indiquant si la ligne est valide
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

  #' @keywords internal
  #' Vérifie la validité d'une colonne et identifie les erreurs éventuelles
  #'
  #' @param col_idx Indice de la colonne à vérifier
  #' @return Booléen indiquant si la colonne est valide
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

  #' @keywords internal
  #' Vérifie la validité de toute la grille
  #'
  #' @return Liste contenant un booléen 'valid' et un message explicatif 'message'
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

  #' @keywords internal
  #' Vérifie si le jeu est terminé et correct
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

  #' @keywords internal
  #' Gestion du bouton de vérification
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

  #' @keywords internal
  #' Gestion du bouton d'indice
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

  #' @keywords internal
  #' Gestion du bouton de solution
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
   observeEvent(input$show_strategy, {
    showModal(modalDialog(
      title = "Stratégies pour Takuzu",
      HTML("
      <ul>
        <li>Évitez les suites de trois chiffres identiques : si vous voyez deux 0 ou deux 1 côte à côte, la case suivante doit forcément être remplie avec l'autre chiffre.</li>
        <li>Veillez à l’équilibre entre 0 et 1 : chaque ligne et chaque colonne doit contenir autant de 0 que de 1. Dès qu’un chiffre est déjà trop présent, complétez les cases restantes avec l’autre.</li>
        <li>Comparez les lignes et les colonnes : si une ligne ou une colonne est presque remplie et ressemble beaucoup à une autre déjà terminée, ajustez les dernières cases pour qu’elles restent différentes.</li>
      </ul>
      <p>Utilisez ces astuces pour progresser dans la résolution de la grille !</p>
    "),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  

  #' @keywords internal
  #' Gestion du bouton nouvelle partie
  observeEvent(input$new_game, {
    initializeGame()
  })
}
