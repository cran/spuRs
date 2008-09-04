ftn <- function(x) {
  return(1.5*sqrt(x))
}

quadrature <- function(ftn, a, b, tol, trace = FALSE) {
  c = (a + b)/2
  fa <- ftn(a)
  fb <- ftn(b)
  fc <- ftn(c)
  h <- (b - a)/2
  Q <- h*(fa + 4*fc + fb)/3
  quad <- quadstep(ftn, a, b, tol, 1, fa, fb, fc, Q, trace)
  quad[2] <- quad[2] + 3
  if (trace) {
    cat("final value is", quad[1], "in", quad[2], "function evaluations\n")
  }
  return(quad)
}

quadstep <- function(ftn, a, b, tol, level, fa, fb, fc, Q0, trace) {
  level.max <- 32
  if (level > level.max) {
    cat("recursion limit reached: singularity likely\n")
    return(NULL)
  } else {
    h <- (b - a)/4
    c <- (a + b)/2
    f1 <- ftn(a + h)
    f2 <- ftn(b - h)
    Q1 <- h*(fa + 4*f1 + fc)/3 #Simpson's rule for left half
    Q2 <- h*(fc + 4*f2 + fb)/3 #Simpson's rule for right half
    Q <- Q1 + Q2 #Q is the new estimate for the integral
    f.count <- 2
    if (trace) {
      cat("[", a, ", ", b, "] level: ", level, ", Q: ", Q, "\n", sep = "")
    }

    if (abs(Q - Q0) > tol) { #if Q not accurate enough, split again
      # do left half the same way, with tol halved
      quad.left <- quadstep(ftn, a, c, tol/2, level + 1, fa, fc, f1, Q1, trace)
      # ... and the right half
      quad.right <- quadstep(ftn, c, b, tol/2, level + 1, fc, fb, f2, Q2, trace)
      Q <- quad.left[1] + quad.right[1]
      f.count <-  f.count + quad.left[2] + quad.right[2];
    } # else we have the answer and return the better estinate Q
    
    return(c(Q, f.count))
  }
}

quad <- quadrature(ftn, 0, 1, 1e-3, trace = TRUE)


