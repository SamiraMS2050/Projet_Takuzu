library(Rcpp)
sourceCpp("takuzu.cpp")
library(shiny)

# Fonction pour créer une grille d'exemple
create_example_grid <- function(size = 6) {
  example <- matrix(c(
    1,0,1,0,1,0,
    0,1,0,1,0,1,
    1,1,0,0,1,0,
    0,0,1,1,0,1,
    1,0,1,0,1,0,
    0,1,0,1,0,1
  ), nrow = size, byrow = TRUE)
  return(example)
}

# Interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Jeu Takuzu - Shiny"),
  
  # Page d'accueil
  uiOutput("home_page"),
  
  # Page de jeu (initialement cachée)
  conditionalPanel(
    condition = "input.start_game",
    sidebarLayout(
      sidebarPanel(
        selectInput("difficulty", "Difficulté :",
                    choices = c("Facile" = "easy",
                                "Moyen" = "medium",
                                "Difficile" = "hard"),
                    selected = "medium"),
        actionButton("new_game", "Nouvelle Grille"),
        actionButton("check_solution", "Vérifier la solution"),
        actionButton("show_rules", "Règles du jeu"),
        textOutput("result"),
        htmlOutput("rules_text")
      ),
      mainPanel(
        uiOutput("takuzu_ui")
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Affichage de la page d'accueil
  output$home_page <- renderUI({
    if (!isTruthy(input$start_game)) {
      fluidRow(
        column(8, offset = 2,
               h2("Bienvenue dans le jeu Takuzu !"),
               p("Voici un exemple de grille Takuzu complète :"),
               renderTable({create_example_grid()}, bordered = TRUE, align = 'c'),
               br(),
               actionButton("start_game", "Commencer le jeu", class = "btn-primary btn-lg")
        )
      )
    }
  })
  
  # Paramètres de difficulté réactifs
  difficulty_params <- reactive({
    switch(input$difficulty,
           "easy" = list(size = 6, fillRate = 0.5),
           "medium" = list(size = 8, fillRate = 0.4),
           "hard" = list(size = 10, fillRate = 0.3))
  })
  
  # Génération de la grille initiale
  initial_grid <- eventReactive(c(input$new_game, input$difficulty), {
    params <- difficulty_params()
    generateTakuzu(params$size, params$fillRate)
  }, ignoreNULL = FALSE)
  
  # Grille réactive modifiable
  takuzu_grid <- reactiveVal()
  
  # Mise à jour de la grille quand les paramètres changent
  observeEvent(initial_grid(), {
    takuzu_grid(initial_grid())
  })
  
  # Affichage de la grille
  output$takuzu_ui <- renderUI({
    params <- difficulty_params()
    grid <- takuzu_grid()
    
    buttons <- lapply(1:params$size, function(row) {
      lapply(1:params$size, function(col) {
        cell_value <- grid[row, col]
        inputId <- paste0("cell_", row, "_", col)
        
        actionButton(
          inputId = inputId,
          label = ifelse(cell_value == -1, " ", as.character(cell_value)),
          class = "takuzu-btn",
          disabled = (initial_grid()[row, col] != -1)
        )
      })
    })
    
    do.call(fluidRow, lapply(buttons, function(row) {
      column(12, do.call(splitLayout, c(row, list(cellWidths = rep("40px", params$size)))))
    }))
  })
  
  # Gestion dynamique des clics
  observe({
    params <- difficulty_params()
    size <- params$size
    
    lapply(1:size, function(row) {
      lapply(1:size, function(col) {
        input_id <- paste0("cell_", row, "_", col)
        
        observeEvent(input[[input_id]], {
          req(takuzu_grid())
          grid <- isolate(takuzu_grid())
          initial <- isolate(initial_grid())
          
          if (initial[row, col] == -1) {
            grid[row, col] <- ifelse(grid[row, col] == 1, 0, 1)
            takuzu_grid(grid)
            updateActionButton(session, input_id, label = as.character(grid[row, col]))
          }
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # Vérification de la solution
  observeEvent(input$check_solution, {
    grid <- takuzu_grid()
    params <- difficulty_params()
    size <- params$size
    
    invalid_rows <- c()
    invalid_cols <- c()
    
    for (i in 1:size) {
      if (sum(grid[i, ] == 0) != size/2 || sum(grid[i, ] == 1) != size/2) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (sum(grid[, i] == 0) != size/2 || sum(grid[, i] == 1) != size/2) {
        invalid_cols <- c(invalid_cols, i)
      }
      
      if (any(rle(grid[i, ])$lengths > 2)) {
        invalid_rows <- c(invalid_rows, i)
      }
      if (any(rle(grid[, i])$lengths > 2)) {
        invalid_cols <- c(invalid_cols, i)
      }
    }
    
    if (length(invalid_rows) + length(invalid_cols) == 0) {
      output$result <- renderText("🎉 Bravo ! Grille correcte !")
    } else {
      msg <- "❌ Erreurs dans :\n"
      if (length(invalid_rows) > 0) msg <- paste0(msg, "Lignes ", paste(invalid_rows, collapse = ", "), "\n")
      if (length(invalid_cols) > 0) msg <- paste0(msg, "Colonnes ", paste(invalid_cols, collapse = ", "))
      output$result <- renderText(msg)
    }
  })
  
  # Afficher les règles
  rules_text <- reactiveValues(text = "")
  observeEvent(input$show_rules, {
    rules_text$text <- "
      <h4>Règles du jeu Takuzu :</h4>
      <ol>
        <li><strong>Équilibre 0/1</strong> : Chaque ligne et colonne doit contenir un nombre égal de 0 et de 1.</li>
        <li><strong>Pas plus de deux identiques consécutifs</strong> : Il ne peut y avoir plus de deux 0 ou deux 1 consécutifs dans une ligne ou colonne.</li>
        <li><strong>Cases pré-remplies</strong> : Certaines cases sont pré-remplies et ne peuvent pas être modifiées.</li>
        <li><strong>Objectif</strong> : Remplir toute la grille en respectant les règles ci-dessus.</li>
      </ol>
    "
  })
  
  output$rules_text <- renderUI({
    HTML(rules_text$text)
  })
}

# Lancer l'application
shinyApp(ui, server)
