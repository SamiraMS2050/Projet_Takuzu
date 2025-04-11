#' Serveur de logique du jeu Takuzu
#'
#' Cette fonction gère la logique du serveur pour un jeu Takuzu interactif développé avec Shiny. Elle initialise le jeu, gère la sélection de la difficulté, les interactions avec les cellules de la grille, valide les lignes et les colonnes, et fournit des indices pour aider à résoudre la grille.
#' Elle inclut également des mécanismes de vérification pour s'assurer que la grille respecte les règles du Takuzu et pour signaler si la grille est complète et correcte.
#'
#' @param input Liste des entrées Shiny, comprenant des informations sur les actions de l'utilisateur (par exemple, la sélection de la difficulté, les clics sur les cellules de la grille, etc.).
#' @param output Liste des sorties Shiny, utilisées pour afficher les résultats du jeu, comme les messages de validation des lignes/colonnes et la grille mise à jour.
#' @param session Objet de session Shiny qui permet de gérer la session de l'utilisateur (par exemple, la gestion des observateurs).
#'
#' @details
#' La fonction gère les aspects suivants du jeu :
#' - La sélection du niveau de difficulté, qui détermine la taille de la grille et le taux de remplissage initial.
#' - L'initialisation du jeu avec une grille générée aléatoirement et une solution correspondante.
#' - La gestion des interactions avec les cellules de la grille (ex. : remplir les cases avec 0 ou 1).
#' - La validation des lignes et des colonnes selon les règles de Takuzu (chaque ligne/colonne doit contenir un nombre égal de 0 et de 1, sans répétition de valeurs).
#' - La fourniture d'indices pour aider l'utilisateur à remplir la grille correctement.
#' - Le suivi de l'état du jeu et l'affichage de messages indiquant si le jeu est terminé et si la grille est correcte.
#'
#' @export
server <- function(input, output, session) {
  # Charge la fonction C++ (sera automatiquement disponible après compilation)
  # Cette ligne n'est pas nécessaire si vous utilisez le système de package R
  # Rcpp::sourceCpp("src/takuzu.cpp")

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
    size(8)  # Grille 8x8 pour niveau difficile
    shinyjs::removeClass("easy", "selected-difficulty")
    shinyjs::removeClass("medium", "selected-difficulty")
    shinyjs::addClass("hard", "selected-difficulty")
  })

  # Fonction pour obtenir le taux de remplissage
  getFillRate <- function() {
    switch(difficulty(),
           "easy" = 0.4,
           "medium" = 0.3,
           "hard" = 0.1)
  }

  # Initialiser le jeu (utilise maintenant la fonction C++ generateTakuzu)
  initializeGame <- function() {
    current_size <- size()
    new_initial <- generateTakuzu(current_size, getFillRate())  # Appel à la fonction C++
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

  # solveTakuzu, isValidPlacement, isValidLine, findErrorsInLine, etc.
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

  # Fonction pour configurer les observateurs de cellule
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
#' Lancer l'application Shiny Takuzu
#'
#' Cette fonction démarre l'application Shiny pour résoudre des grilles du jeu Takuzu. Elle combine l'interface utilisateur (UI) et le serveur (logique de gestion du jeu) et lance l'application dans le navigateur de l'utilisateur.
#'
#' @details
#' La fonction utilise la fonction `shinyApp` pour créer une application Shiny à partir des objets `ui` et `server`. L'interface utilisateur permet à l'utilisateur d'interagir avec une grille Takuzu, tandis que le serveur gère la logique du jeu.
#'
#' @return
#' Aucune valeur retournée. La fonction lance l'application dans le navigateur.
#'
#' @examples
#' # Pour démarrer l'application Shiny Takuzu, il suffit d'appeler :
#' runTakuzuApp()
#'

runTakuzuApp <- function() {


  shinyApp(ui, server)

}
