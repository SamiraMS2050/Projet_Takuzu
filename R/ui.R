#' Interface utilisateur pour l'application Takuzu
#'
#' Cette fonction génère l'interface utilisateur complète pour le jeu Takuzu, comprenant
#' une page d'accueil avec les règles et la sélection de difficulté, ainsi qu'une page de jeu
#' avec une grille interactive et des contrôles pour vérifier, obtenir des indices et afficher la solution.
#'
#' @return Un objet Shiny UI
#' @import shiny
#' @importFrom shinyjs useShinyjs hidden
#' @export
#'
#' @examples
#' \dontrun{
#' # Utilisation avec shinyApp
#' shinyApp(ui = takuzu_ui(), server = takuzu_server)
#' }
takuzu_ui <- function() {
  fluidPage(
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
      h1("Jeu Takuzu", class = "takuzu"),
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
          tags$li("Facile : Grille de taille (6×6) remplie à 40%"),
          tags$li("Moyen : Grille de taille (8×8) remplie à 30%"),
          tags$li("Difficile : Grille de taille (8×8) remplie à 10%")
        ),
        actionButton("easy", "Facile", class = "difficulty-btn"),
        actionButton("medium", "Moyen", class = "difficulty-btn"),
        actionButton("hard", "Difficile", class = "difficulty-btn")
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
}
