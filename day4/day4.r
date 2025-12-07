input <- readr::read_lines(here::here("day4/input.txt")) |>
  lapply(\(x) strsplit(x, ""))
m <- length(input)
n <- length(input[[1]][[1]])
input_mat <- matrix(unlist(input), nrow = m, ncol = n, byrow = TRUE)


## Part 1
get_adj_pos <- function(i, j) {
  prod <- expand.grid(c(i, i + 1, i - 1), c(j, j + 1, j - 1))
  prod <- prod[!(prod[[1]] == i & prod[[2]] == j), ]
  return(list(prod[[1]], prod[[2]]))
}
check_pos <- function(x, i, j, m, n) {
  if (i > m || j > n || i < 1 || j < 1) {
    return(NA)
  } else {
    return(x[i, j] %in% c("@", "x"))
  }
}
mark_rolls <- function(input_mat) {
  m <- nrow(input_mat)
  n <- ncol(input_mat)
  for (i in 1:m) {
    for (j in 1:n) {
      if (input_mat[i, j] != "@") {
        next
      }
      pos <- get_adj_pos(i, j)
      lgl <- map2_lgl(pos[[1]], pos[[2]], \(p, q) {
        return(check_pos(input_mat, p, q, m, n))
      })
      if (sum(lgl, na.rm = TRUE) < 4) input_mat[i, j] <- "x"
    }
  }
  return(input_mat)
}

part1 <- mark_rolls(input_mat)
part1 <- sum(part1 == "x")
print(part1)

## Part 2
remove_rolls <- function(input_mat) {
  new_mat <- mark_rolls(input_mat)
  new_mat[new_mat == "x"] <- "."
  return(new_mat)
}

current_mat <- input_mat
repeat {
  new_mat <- remove_rolls(current_mat)
  if (identical(current_mat, new_mat)) {
    break
  }
  current_mat <- new_mat
}
part2 <- (sum(input_mat == "@") - sum(current_mat == "@"))
print(part2)
