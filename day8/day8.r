input <- readr::read_csv("day8/input.txt", col_names = c("x", "y", "z"))


## Part 1
dist_mat <- as.matrix(dist(input, method = "euclidean"))
idx <- which(lower.tri(dist_mat), arr.ind = TRUE)
edges <- data.frame(i = idx[, 1], j = idx[, 2], d = dist_mat[idx]) |>
  dplyr::arrange(d)
