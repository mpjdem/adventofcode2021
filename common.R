#' Compose the file path of a solution file
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#'
#' @return A `character` string
#'
solution_fpath <- function(day, part) {

  stopifnot(day %in% 1:25)
  stopifnot(part %in% 1:2)

  file.path("output", paste0("output", day, "_", part, ".txt"))

}

#' Check that a solution correct, against a file in `output/`
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#' @param solution The answer to check
#'
#' @return `NULL`
#'
check_solution <- function(day, part, solution) {
  cat(paste0("Solution to Part ", part, ": ", solution, " - "))
  correct_solution <- readLines(solution_fpath(day, part))
  if (as.character(solution) == correct_solution) cat("correct!\n") else cat("wrong!\n")
  invisible()
}

#' Write a solution as the correct answer in `output/`
#'
#' @param day Which day of AoC (1-25)
#' @param part Which part of the day (1 or 2)
#' @param solution The answer to write
#'
#' @return `NULL`
#'
write_solution <- function(day, part, solution) {
  writeLines(as.character(solution), solution_fpath(day, part))
  invisible()
}
