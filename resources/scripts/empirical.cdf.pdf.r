# program: empirical.cdf.pdf.r

empirical.cdf <- function(x, range = c(min(x), max(x))) {
  # plot empirical cdf of sample x over the interval given by range
  n <- length(x)
  x <- sort(x)
  x.plot <- range[1]
  y.plot <- c(0, 0)
  for (i in 1:n) {
    x.plot <- c(x.plot, x[i], x[i])
    y.plot <- c(y.plot, i/n, i/n)
  }
  x.plot <- c(x.plot, range[2])
  plot(x.plot, y.plot, type='l', xlab='', ylab='')
}

scaled.hist1 <- function(x, delta) {
  # plot a scaled histogram of the sample x using bins of width delta
  # calculate bin edges
  edges <- seq(from=delta*(min(x)%/%delta), to=delta*(max(x)%/%delta+1), by=delta)
  n.bins <- length(edges) - 1
  # count number of x's in each bin
  counts <- rep(0, times=n.bins)
  for (i in 1:length(x)) {
    # find the bin j containing x[i]
    j <- 1
    while (edges[j+1] < x[i]) {
      j <- j + 1
    }
    # add one to count for j-th bin
    counts[j] <- counts[j] + 1
  }
  # for k-th bin plot a box of width delta and height counts[k]
  x.plot <- c()
  y.plot <- c()
  for (i in 1:n.bins) {
    x.plot <- c(x.plot, c(edges[i], edges[i], edges[i+1], edges[i+1], edges[i]))
    y.plot <- c(y.plot, c(0, counts[i], counts[i], 0, 0)/delta/length(x))
  }
  plot(x.plot, y.plot, type='l', xlab='', ylab='')
}

scaled.hist2 <- function(x, delta) {
  # plot a scaled histogram of the sample x using bins of width delta
  # scale x so that observations in the k-th bin are labelled k
  x <- floor(x/delta)
  min.x <- min(x)
  x <- x - min.x + 1
  # count number of x's in each bin
  counts <- rep(0, max(x))
  for (i in 1:length(x)) {
    counts[x[i]] <- counts[x[i]] + 1
  }
  # for k-th bin plot a box of width delta and height counts[k]
  x.plot <- c()
  y.plot <- c()
  for (i in 1:length(counts)) {
    x.plot <- c(x.plot, delta*c(i-1, i-1, i, i, i-1))
    y.plot <- c(y.plot, c(0, counts[i], counts[i], 0, 0)/delta/length(x))
  }
  x.plot <- x.plot + delta*min.x
  plot(x.plot, y.plot, type='l', xlab='', ylab='')
}

cavendish <- c(5.50, 5.57, 5.42, 5.61, 5.53, 5.47, 4.88, 5.62, 5.63,
4.07, 5.29, 5.34, 5.26, 5.44, 5.46, 5.55, 5.34, 5.30, 5.36,
5.79, 5.75, 5.29, 5.10, 5.86, 5.58, 5.27, 5.85, 5.65, 5.39)
par(mfrow = c(2, 1))
empirical.cdf(cavendish)
title(xlab='density of the earth', ylab='cumulative freq')
scaled.hist1(cavendish, .08)
title(xlab='density of the earth', ylab='scaled hist')
par(mfrow = c(1, 1))
