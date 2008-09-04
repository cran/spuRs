# Sierpinski-gasket filling curve
#
# vector x contains instructions "L", "R" and "A"
# for "turn left 60 degrees", "turn right 60 degrees" and "advance 1 pace"
#
# at stage n x contains instructions for a level n approximation to the curve
# function iterate takes a stage n curve and produces a stage n+1 curve
#
# function flip swaps the left-right orientation of a curve
#
# function trim removes redundant left-right pairs in a curve
#
# function gasket defines the level 1 curve then applies iterate
#
# function plot.gasket translates the curve vector into an array of
# spatial positions called state, each row of which gives x and y co-ordinates,
# and a heading in radians, which is then plotted

iterate <- function(x) {
  x <- c(flip(x), "L", x, "L", flip(x))
  return(trim(x))
}

flip <- function(x) {
  Lidx <- x == "L"
  Ridx <- x == "R"
  x[Lidx] <- "R"
  x[Ridx] <- "L"
  return(x)
}

trim <- function(x) {
  i <- 1
  while (i < length(x)) {
    if (x[i] == "L" & x[i+1] == "R") {
      x <- x[-c(i, i+1)]
    } else if (x[i] == "R" & x[i+1] == "L") {
      x <- x[-c(i, i+1)]
    } else {
      i <- i + 1
    }
  }
  return(x)
}

gasket <- function(n) {
  x <- c("A")
  if (n > 0) {
    for (i in 1:n) x <- iterate(x)
  }
  if (n %% 2 == 0) x <- c("L", x)
  class(x) <- "gasket"
  return(x)
}

plot.gasket <- function(x, ...) {
  state <- matrix(0, nrow=1, ncol=3) #(x,y,radians)
  n <- 1
  for (i in 1:length(x)) {
    if (x[i] == "L") {
      state[n,3] <- state[n,3] + pi/3
    } else if (x[i] == "R") {
      state[n,3] <- state[n,3] - pi/3
    } else {
      state <- rbind(state, state[n,] + c(cos(state[n,3]),sin(state[n,3]),0))
      n <- n+1
    }
  }
  plot(state[,1], state[,2], type="l", asp=1, ...)
}

par(mfrow=c(4,2), mar=c(1,1,4,1))
plot(gasket(0), xlab="", ylab="", main="gasket iteration 0", axes=F)
plot(gasket(1), xlab="", ylab="", main="gasket iteration 1", axes=F)
plot(gasket(2), xlab="", ylab="", main="gasket iteration 2", axes=F)
plot(gasket(3), xlab="", ylab="", main="gasket iteration 3", axes=F)
plot(gasket(4), xlab="", ylab="", main="gasket iteration 4", axes=F)
plot(gasket(5), xlab="", ylab="", main="gasket iteration 5", axes=F)
plot(gasket(6), xlab="", ylab="", main="gasket iteration 6", axes=F, lwd=0.5)
plot(gasket(7), xlab="", ylab="", main="gasket iteration 7", axes=F, lwd=0.5)
par(mfrow=c(1,1))

par(mfrow=c(4,3), mar=c(1,1,4,1))
plot(gasket(0), xlab="", ylab="", main="gasket iteration 0", axes=F, lwd=2)
plot(gasket(1), xlab="", ylab="", main="gasket iteration 1", axes=F, lwd=2)
plot(gasket(2), xlab="", ylab="", main="gasket iteration 2", axes=F, lwd=2)
plot(gasket(3), xlab="", ylab="", main="gasket iteration 3", axes=F)
plot(gasket(4), xlab="", ylab="", main="gasket iteration 4", axes=F)
plot(gasket(5), xlab="", ylab="", main="gasket iteration 5", axes=F)
plot(gasket(6), xlab="", ylab="", main="gasket iteration 6", axes=F, lwd=0.5)
plot(gasket(7), xlab="", ylab="", main="gasket iteration 7", axes=F, lwd=0.5)
plot(gasket(8), xlab="", ylab="", main="gasket iteration 8", axes=F, lwd=0.25)
plot(gasket(9), xlab="", ylab="", main="gasket iteration 9", axes=F, lwd=0.25)
plot(gasket(10), xlab="", ylab="", main="gasket iteration 10", axes=F, lwd=0.1)
plot(gasket(11), xlab="", ylab="", main="gasket iteration 11", axes=F, lwd=0.1)
par(mfrow=c(1,1))
