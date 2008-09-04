max.age <- 0
max.height <- 0
for (i in 1:length(trees)) {
  if (max(trees[[i]]$age) > max.age) 
    max.age <- max(trees[[i]]$age)
  if (max(trees[[i]]$height) > max.height) 
    max.height <- max(trees[[i]]$height)
}

my.max <- function(x, i) max(x[[i]]) #max of element i of list x
max.age <- max(sapply(trees, my.max, "age"))
max.height <- max(sapply(trees, my.max, "height.ft"))

plot(c(0, max.age), c(0, max.height), type="n", xlab="age (years)", ylab="height (feet)")
for (i in 1:length(trees)) 
  lines(trees[[i]]$age, trees[[i]]$height.ft)
