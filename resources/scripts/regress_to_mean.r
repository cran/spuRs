next.gen <- function(pop) {
  pop$m <- sample(pop$m)
  pop$m <- apply(pop, 1, mean)
  pop$f <- pop$m
  return(pop)
}

set.seed(329)

pop <- data.frame(m = rnorm(100, 160, 20), f = rnorm(100, 160, 20))
n.gen <- 9
x.gen <- data.frame(ht = pop$m, gen = rep(1, length(pop$m)))
for (n in 1:n.gen) {
  pop <- next.gen(pop)
  x.gen <- data.frame(ht = c(x.gen$ht, pop$m),
    gen = c(x.gen$gen, rep(n, length(pop$m))))
}

require(lattice)
print(
  histogram(~ ht | gen, data = x.gen,
    as.table = T, main = "Distribution of male height by generation")
)
