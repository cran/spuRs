fmin.nm <- function(f, grad.f, hess.f, x0, tol = 1e-9) {
    # find a local minimum of f near the point x0
    # let d = length(x0) then f is a function that takes
    #   a vector of length d and returns a real number
    # grad.f is a function that takes a vector of length d
    #   and returns a vector of length d, which should be
    #   the gradient vector of f
    # hess.f is a function that takes a vector of length d
    #   and returns a square matrix of size d*d, which should be
    #   the Hessian of f
    # algorithm stops when successive approximations are within tol 
    #   in each co-ordinate

    x <- x0
    x.old <- x + 2*tol # so while loop executes at least once
    while (max(abs(x - x.old)) > tol) {
        x.old <- x
        x <- x - solve(hess.f(x), grad.f(x))
    }
    return(x)
}
