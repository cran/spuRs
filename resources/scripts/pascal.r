pascal <- function(n) {
  # returns Pascal's triangle to depth n
  if (n == 1) {
    return(list(c(1, 1)))
  } else {
    pt <- pascal(n-1)
    pt1 <- pt[[n-1]]
    pt2 <- c(1, pt1[1:(length(pt1)-1)]+pt1[2:length(pt1)], 1)
    pt[n] <- list(pt2)
    return(pt)
  }
}

  