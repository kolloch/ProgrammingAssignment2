# Sample matrix. Generated randomly and then dumped.
A <-
  structure(c(1+0i, 0.5+0i, 0.333333333333333+0i, 0.25+0i, 0.5+0i, 
              0.333333333333333+0i, 0.25+0i, 0.2+0i, 0.333333333333333+0i, 
              0.25+0i, 0.2+0i, 0.166666666666667+0i, 0.25+0i, 0.2+0i, 0.166666666666667+0i, 
              0.142857142857143+0i), .Dim = c(4L, 4L))

test.getSet <- function() {
  cachable <- makeCacheMatrix(A)
  
  A2 <- cachable$get()
  checkIdentical(A, A2)

  B <- matrix()
  cachable$set(B)
  B2 <- cachable$get()
  checkIdentical(B, B2)
}

test.cacheSolveWithDifferentInput <- function() {
  
  cachable <- makeCacheMatrix(A)
  solveA <- cacheSolve(cachable)
  checkIdentical(solve(A), solveA)
  solveA2 <- cacheSolve(cachable)
  checkIdentical(solveA, solveA2)
  
  M <- matrix(c(1,4,5,2,4,5,2,1,2), nrow=3, ncol=3)
  cachableM <- makeCacheMatrix(M)
  solvedM <- cacheSolve(cachableM)
  checkIdentical(solve(M), solvedM)
}

test.cacheSolveWithDifferentParameters <- function() {
  cachable <- makeCacheMatrix(A)
  solved1a <- cacheSolve(cachable, c(1,2,3,4))
  checkIdentical(solve(A, c(1,2,3,4)), solved1a)
  solved1b <- cacheSolve(cachable, c(1,2,3,4))
  checkIdentical(solved1a, solved1b)
  # just make sure that this also works for the test below
  checkTrue(identical(solved1a, solved1b))
  
  # Now we call cacheSolve with other parameters.
  # We do NOT want the cached solution because that would be wrong.
  solved2a <- cacheSolve(cachable)
  checkTrue(!identical(solved1b, solved2a))
  solved2b <- cacheSolve(cachable)
  checkIdentical(solved2a, solved2b)
}