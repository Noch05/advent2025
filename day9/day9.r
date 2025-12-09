# Prepping and Splitting Coordinates
input <- readr::read_lines("day9/input.txt") |>
  strsplit(",") |>
  lapply(as.numeric)

x_coords <- vapply(input, \(x) x[1], FUN.VALUE = numeric(1))
y_coords <- vapply(input, \(x) x[2], FUN.VALUE = numeric(1))

## Part 1

x_mat <- abs(outer(x_coords, x_coords, `-`)) + 1
y_mat <- abs(outer(y_coords, y_coords, `-`)) + 1
area_mat <- x_mat * y_mat
part1 <- max(area_mat)
