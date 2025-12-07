options(scipen = 999)
input <- readr::read_lines("day7/input.txt") |>
  strsplit("")
m <- length(input)
n <- length(input[[1]])
input_mat <- matrix(unlist(input), ncol = n, nrow = m, byrow = TRUE)
input_mat[2, 71] <- "|"

## Part 1

x <- input_mat
splits <- 0
for (i in 2:m) {
  for (j in 1:n) {
    if (x[i - 1, j] == "|" && x[i, j] == ".") {
      x[i, j] <- "|"
    }
    if (x[i, j] == "^" && x[i - 1, j] == "|") {
      x[i, j - 1] <- "|"
      x[i, j + 1] <- "|"
      splits <- splits + 1
    }
  }
}

print(splits)

## Part 2
path_mat <- matrix(0, m, n)
path_mat[c(1, 2), 71] <- 1
for (i in 1:m) {
  for (j in 1:n) {
    k <- path_mat[i, j]
    if (x[i, j] == "|" && i < m) {
      path_mat[i + 1, j] <- path_mat[i + 1, j] + k
    } else if (
      x[i, j] == "^" && x[i - 1, j] == "|" && i < m && j > 1 && j < n
    ) {
      path_mat[i + 1, j - 1] <- path_mat[i + 1, j - 1] + k
      path_mat[i + 1, j + 1] <- path_mat[i + 1, j + 1] + k
    }
  }
}
part2 <- sum(path_mat[nrow(path_mat), ])
print(part2)
