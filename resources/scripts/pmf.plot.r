pmf.plot <- function(x, p) {
  plot(c(min(x), max(x)), c(0,1), type = "n", xlab = "x", ylab = "P(X=x)",
    main = paste("mean = ", sum(x*p), " and variance = ", round(sum(x*x*p) - (sum(x*p))^2, 2), sep = ""))
  for (j in 1:length(x)) {
    lines(c(x[j], x[j]), c(0, p[j]))
  }
}
