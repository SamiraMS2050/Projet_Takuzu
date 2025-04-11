#' Exécuter l'application Takuzu
#'
#' Cette fonction lance l'application Shiny Takuzu
#'
#' @export
run_takuzu_app <- function() {
  appDir <- system.file("takuzuApp", package = "takuzuGame")
  if (appDir == "") {
    stop("Impossible de trouver le répertoire de l'application. Essayez de réinstaller le package.")
  }

  shinyApp(
    ui = takuzu_ui(),
    server = takuzuServer,
    options = list(
      launch.browser = TRUE
    )
  )
}
