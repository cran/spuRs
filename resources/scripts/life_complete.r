# spuRs/resources/scripts/life_complete.r

neighbours <- function(A, i, j, n) {
    # A is an n*n 0-1 matrix
    # calculate number of neighbours of A[i,j]
    #
    # index neighbours as follows
    #
    # t,l t,m t,r
    # a,l i,j a,r
    # b,l b,m b,r
    #
    # indices are wrapped as on a torus

    if (i == 1) t <- n else t <- i - 1
    a <- i
    if (i == n) b <- 1 else b <- i + 1
    
    if (j == 1) l <- n else l <- j - 1
    m <- j
    if (j == n) r <- 1 else r <- j + 1

    return(A[t,l] + A[t,m] + A[t,r] + A[a,l] + A[a,r] + A[b,l] + A[b,m] + A[b,r])
}

glidergun <- function(n) {
    # initial n*n lattice for a glider gun
    # assumes n >= 40
    A <- matrix(0, n, n)
    A[31,13] <- 1
    A[31,14] <- 1
    A[32,12] <- 1
    A[32,16] <- 1
    A[33,11] <- 1
    A[33,17] <- 1
    A[33,25] <- 1
    A[34,1] <- 1
    A[34,2] <- 1
    A[34,11] <- 1
    A[34,15] <- 1
    A[34,17] <- 1
    A[34,18] <- 1
    A[34,23] <- 1
    A[34,25] <- 1
    A[35,1] <- 1
    A[35,2] <- 1
    A[35,11] <- 1
    A[35,17] <- 1
    A[35,21] <- 1
    A[35,22] <- 1
    A[36,12] <- 1
    A[36,16] <- 1
    A[36,21] <- 1
    A[36,22] <- 1
    A[36,35] <- 1
    A[36,36] <- 1
    A[37,13] <- 1
    A[37,14] <- 1
    A[37,21] <- 1
    A[37,22] <- 1
    A[37,35] <- 1
    A[37,36] <- 1
    A[38,23] <- 1
    A[38,25] <- 1
    A[39,25] <- 1
    return(A)
}

# grid size
n <- 50

# initialise lattice
#A <- matrix(round(runif(n^2)), n, n)
A <- glidergun(n)

finished <- FALSE
while (!finished) {
    # plot
    plot(c(1,n), c(1,n), type = "n", xlab = "", ylab = "")
    for (i in 1:n) {
        for (j in 1:n) {
            if (A[i,j] == 1) {
                points(i, j)
            }
        }
    }
    
    # update
    B <- A
    for (i in 1:n) {
        for (j in 1:n) {
            nbrs <- neighbours(A, i, j, n)
            if (A[i,j] == 1) {
                if ((nbrs == 2) | (nbrs == 3)) {
                    B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            } else {
                if (nbrs == 3) {
                    B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            }
        }
    }
    A <- B
    
    # continue
    #input <- readline("stop? ")
    #if (input == "y") finished <- TRUE
}
