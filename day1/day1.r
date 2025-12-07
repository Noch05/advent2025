input <- readr::read_lines(here::here("day1/input.txt"))

rotate_safe <- function(dir, current_pos, dist) {
  sign <- ifelse(dir == "R", 1, -1)
  new_pos <- (current_pos + (sign * (dist %% 100))) %% 100
  return(new_pos)
}

cross_zero <- function(old_pos, new_pos, dir, dist) {
  sign <- ifelse(dir == "R", 1, -1)
  turns <- dist %/% 100
  remainder <- dist %% 100
  extra <- if (new_pos == 0) {
    1
  } else if (
    remainder > 0 && (sign * (new_pos - old_pos) < 0 && old_pos != 0)
  ) {
    1
  } else {
    0
  }
  return(turns + extra)
}

positions_vec <- c(50)
zero_cross <- 0
i <- 1
for (line in input) {
  dist <- as.numeric(stringr::str_extract(line, "\\d+"))
  dir <- substr(line, 1, 1)
  positions_vec[[i + 1]] <- rotate_safe(dir, positions_vec[[i]], dist)
  zero_cross <- zero_cross +
    cross_zero(positions_vec[[i]], positions_vec[[i + 1]], dir, dist)
  i <- i + 1
}
part1 <- sum(positions_vec == 0)
part2 <- zero_cross
print(c(part1, part2))
