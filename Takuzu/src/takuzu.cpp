#include <Rcpp.h>
using namespace Rcpp;
// Vérifie si la grille respecte les règles du Takuzu
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
      if (isValid(grid, row, col, value, size)) {
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
