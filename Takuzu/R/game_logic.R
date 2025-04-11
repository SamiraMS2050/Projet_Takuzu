
#' Solve Takuzu grid
#'
#' Résout une grille de Takuzu en utilisant une approche récursive de recherche de solutions.
#' Retourne la grille résolue ou `NULL` si aucune solution n'est trouvée.
#'
#' @param grid Grille à résoudre (matrice d'entiers).
#' @return La grille résolue ou `NULL` si aucune solution n'est trouvée.
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

#' Check if placement is valid
#'
#' Cette fonction vérifie si la placement d'une valeur (0 ou 1) dans une case spécifique de la grille respecte les règles de Takuzu.
#' Elle s'assure que les lignes et les colonnes ne contiennent pas plus de la moitié de 0 ou de 1, et qu'aucune ligne ou colonne n'est identique à une autre.
#'
#' @param grid Grille actuelle (matrice d'entiers).
#' @param row Index de la ligne (un entier).
#' @param col Index de la colonne (un entier).
#' @param val Valeur à vérifier (0 ou 1).
#' @return Vrai si le placement est valide, sinon Faux.
#' @export
# Exemple dans la fonction isValidPlacement :
isValidPlacement <- function(grid, row, col, val) {
  current_size <- nrow(grid)  # Utiliser nrow() au lieu de size()

  if (col > 2 && grid[row, col-1] == val && grid[row, col-2] == val) return(FALSE)
  if (row > 2 && grid[row-1, col] == val && grid[row-2, col] == val) return(FALSE)

  zero_count_row <- sum(grid[row, ] == 0, na.rm = TRUE)
  zero_count_col <- sum(grid[, col] == 0, na.rm = TRUE)

  if (val == 0 && (zero_count_row >= current_size/2 || zero_count_col >= current_size/2)) return(FALSE)

  return(TRUE)
}
