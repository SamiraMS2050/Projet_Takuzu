#' Get fill rate based on difficulty
#' @param difficulty Game difficulty
#' @return Fill rate
#' @export
getFillRate <- function(difficulty) {
  switch(difficulty,
         "easy" = 0.4,
         "medium" = 0.3,
         "hard" = 0.1)
}

#' Initialize new game
#' @param size Grid size
#' @param difficulty Game difficulty
#' @export
initializeGame <- function(size, difficulty) {
  fill_rate <- getFillRate(difficulty)
  grid <- generateTakuzu(size, fill_rate)
  solution <- solveTakuzu(grid)

  list(
    initial_grid = grid,
    current_grid = grid,
    solution = solution,
    size = size
  )
}
