#include <Rcpp.h>
using namespace Rcpp;

/**
 * @title Vérifie la validité d'un mouvement dans une grille Takuzu
 * @description Vérifie si placer une valeur (0 ou 1) à une position donnée respecte les règles du Takuzu
 * @param grid La grille Takuzu actuelle
 * @param row La ligne où placer la valeur
 * @param col La colonne où placer la valeur
 * @param value La valeur à placer (0 ou 1)
 * @param size La taille de la grille
 * @return Un booléen indiquant si le mouvement est valide
 */
bool isValid(IntegerMatrix grid, int row, int col, int value, int size) {
  if (col > 1 && grid(row, col-1) == value && grid(row, col-2) == value)
    return false;
  if (col < size-2 && grid(row, col+1) == value && grid(row, col+2) == value)
    return false;
  if (row > 1 && grid(row-1, col) == value && grid(row-2, col) == value)
    return false;
  if (row < size-2 && grid(row+1, col) == value && grid(row+2, col) == value)
    return false;

  int rowZeros = 0, rowOnes = 0, colZeros = 0, colOnes = 0;
  for (int i = 0; i < size; i++) {
    if (grid(row, i) == 0) rowZeros++;
    if (grid(row, i) == 1) rowOnes++;
    if (grid(i, col) == 0) colZeros++;
    if (grid(i, col) == 1) colOnes++;
  }
  if ((rowZeros >= size/2 && value == 0) || (rowOnes >= size/2 && value == 1))
    return false;
  if ((colZeros >= size/2 && value == 0) || (colOnes >= size/2 && value == 1))
    return false;

  return true;
}

//'  @name generateTakuzu
 //' @title Génère une grille de Takuzu partiellement remplie
 //' @description Crée une grille de Takuzu de taille spécifiée avec un taux de remplissage défini
 //' @param size La taille de la grille (nombre de lignes et de colonnes)
 //' @param fillRate Le taux de remplissage de la grille (entre 0 et 1)
 //' @return Une matrice d'entiers représentant la grille de Takuzu générée
 //' @details
 //' Cette fonction génère une grille de Takuzu partiellement remplie en respectant
 //' les règles du jeu : pas plus de deux 0 ou deux 1 consécutifs, et autant de 0 que de 1
 //' dans chaque ligne et colonne. Les cellules non remplies sont marquées par -1.
 //' @examples
 //' \dontrun{
 //' # Génère une grille 8x8 avec 40% des cellules remplies
 //' grid <- generateTakuzu(8, 0.4)
 //' print(grid)
 //' }
 //' @export
 // [[Rcpp::export]]
 IntegerMatrix generateTakuzu(int size, double fillRate) {
   if (fillRate < 0 || fillRate > 1)
     stop("fillRate doit être entre 0 et 1");

   IntegerMatrix grid(size, size);
   std::fill(grid.begin(), grid.end(), -1);

   int filledCells = (int)(size * size * fillRate);
   int count = 0;

   while (count < filledCells) {
     int row = rand() % size;
     int col = rand() % size;

     if (grid(row, col) == -1) {
       int value = rand() % 2;
       if (isValid(grid, row, col, value, size)) {  // Parenthèse fermée ici
         grid(row, col) = value;
         count++;
       }
     }
   }

   return grid;
 }

/*** R
generateTakuzu(8, 0.4)
*/
