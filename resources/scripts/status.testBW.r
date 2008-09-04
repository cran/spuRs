# Program spuRs/resources/scripts/status.testBW.r

status.testBW <- function(s.ftn) {
  x.vec <- (-1):11
  y.vec <- (-1):11
  plot(x.vec, y.vec, type = "n", xlab = "x", ylab = "y")
  for (x in x.vec) {
    for (y in y.vec) {
      s <- s.ftn(x, y)
      if (s == "impossible") text(x, y, "X")
      else if (s == "unfinished") text(x, y, "?")
      else if (s == "player 1 win") text(x, y, "1")
      else if (s == "player 2 win") text(x, y, "2")
    }
  }
  return(invisible(NULL))
}
