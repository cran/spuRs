status0 <- function(x, y) {
  if (x < 0) {
    return("impossible")
  } else if (x < 8) {
    if (y < 0) {
      return("impossible")
    } else if (y < 9) {
      return("unfinished")
    } else if (y == 9) {
      return("player 2 win")
    } else {
      return("impossible")
    }
  } else if (x == 8) {
    if (y < 0) {
      return("impossible")
    } else if (y < 10) {
      return("unfinished")
    } else if (y == 10) {
      return("player 2 win")
    } else {
      return("impossible")
    }
  } else if (x == 9) {
    if (y < 0) {
      return("impossible")
    } else if (y <= 7) {
      return("player 1 win")
    } else if (y <= 10) {
      return("unfinished")
    } else if (y == 11) {
      return("player 2 win")
    } else {
      return("impossible")
    }
  } else {
    if (y < x - 2) {
      return("impossible")
    } else if (y == x - 2) {
      return("player 1 win")
    } else if (y <= x + 1) {
      return("unfinished")
    } else if (y == x + 2) {
      return("player 2 win")
    } else {
      return("impossible")
    }
  }
}

status <- function(x, y) {
  if ((x < 0) | (y < 0)) {
    return("impossible")
  } else if (((x <= 8) & (y <= 8)) | ((x <= y+1) & (y <= x+1))) {
    return("unfinished")
  } else if (((x == 9) & (y <= 7)) | ((x == y+2) & (y >= 8))) {
    return("player 1 win")
  } else if (((y == 9) & (x <= 7)) | ((y == x+2) & (x >= 8))) {
    return("player 2 win")
  } else {
    return("impossible")
  }
}
