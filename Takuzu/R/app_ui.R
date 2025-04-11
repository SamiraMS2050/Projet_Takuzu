#' Takuzu Game UI
#'
#' Crée l'interface utilisateur pour le jeu Takuzu. Cette fonction génère la structure de la page Shiny, y compris la présentation des règles, les niveaux de difficulté et les options de jeu.
#' Elle inclut également le style CSS pour personnaliser l'apparence de l'interface et définit l'agencement des éléments de jeu.
#'
#' @return UI Shiny pour le jeu Takuzu
#' @export
ui <- function() {
  fluidPage(
    # Inclure le fichier CSS depuis le répertoire inst/www
    includeCSS(system.file("www", "styles.css", package = "TakuzuGame")),

    fluidRow(
      column(12, h2("Bienvenue dans le jeu Takuzu"))
    ),
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      #game_page {
        background: linear-gradient(135deg, #6e8efb, #a777e3);
        color: black;
        padding: 20px;
        min-height: 100vh;
      }
      /* [Tous vos autres styles CSS exactement comme dans votre code original] */
      "))
    ),

    div(
      id = "welcome_page",
      h1("Jeu Takuzu", class="takuzu"),
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
}
