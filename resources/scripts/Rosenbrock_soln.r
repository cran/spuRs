# Solution to Rosenbrock exercise.

source('ascent.r')

g <- function(x) -(1 - x[1])^2 - 100*(x[2] - x[1]^2)^2
gradg <- function(x) c(2*(1 - x[1]) + 400*(x[2] - x[1]^2)*x[1], -200*(x[2] + x[1]^2))

ascent <- function(f, grad.f, x0, tol = 1e-9, n.max = 100) {
    # steepest ascent algorithm
    # find a local max of f starting at x0
    # function grad.f is the gradient of f

    x <- x0
    points(x[1], x[2], pch=19)
    x.old <- x
    x <- line.search(f, x, grad.f(x))
    n <- 1
    while ((f(x) - f(x.old) > tol) & (n < n.max)) {
        lines(c(x.old[1], x[1]), c(x.old[2], x[2]))
        points(x[1], x[2])
        x.old <- x
        x <- line.search(f, x, grad.f(x))
        n <- n + 1
    }
    return(x)
}

ascent(g, gradg, c(0, 3), n.max=10000)

newton <- function(f3, x0, tol = 1e-9, n.max = 100) {
    # Newton's method for optimisation, starting at x0
    # f3 is a function that given x returns the list
    # {f(x), grad f(x), Hessian f(x)}, for some f

    x <- x0
    f3.x <- f3(x)
    n <- 0
    points(x[1], x[2], pch=19)
    while ((max(abs(f3.x[[2]])) > tol) & (n < n.max)) {
        x.old <- x
        x <- x - solve(f3.x[[3]], f3.x[[2]])
        f3.x <- f3(x)
        n <- n + 1
        lines(c(x.old[1], x[1]), c(x.old[2], x[2]))
        points(x[1], x[2])
    }
    if (n == n.max) {
        cat('newton failed to converge\n')
    } else {
        return(x)
    }
}

newton(Rosenbrock, c(0, 3))
