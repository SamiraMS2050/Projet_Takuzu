

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
#' @export
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
#' @export
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
#' @export
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
#' @export
findErrorsInLine <- function(line) {
  errors <- rep(FALSE, length(line))
  for (i in 2:(length(line)-1)) {
    if (line[i-1] == line[i] && line[i] == line[i+1]) {
      errors[(i-1):(i+1)] <- TRUE
    }
  }
  return(errors)
}
