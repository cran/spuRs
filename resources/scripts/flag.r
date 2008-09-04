x <- c(2, 5, 6, 9, 7, 0, 7, 0, 3, 1)
y <- 3
i <- 0
found <- FALSE
while (i < length(x) && !found) {
  i <- i + 1
  if (x[i] == y) {
    found <- TRUE
  }
}
if (found) {
  cat(y, "found in position", i, "\n")
} else {
  cat(y, "not found\n")
}