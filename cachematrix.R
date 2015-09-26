## This file provide two functions which are supposed to be used in combination.
## makeCacheMatrix is used to setup a list which contains a matrix and
## a cached result value of the solve method along the parameters passed to the solve method.
## cacheSolve will inspect the object for a matching cached value. 
## If it finds a matching cache value, it will return it. Otherwise
## it will call the solve method an cache its result.
##
## Example for a given matrix A:
##   cachable <- makeCacheMatrix(A)
##   # this will call the solve function internally
##   solved1a <- cacheSolve(cachable)
##   # this will use the cached value and not call the solve function again
##   solved1b <- cacheSolve(cachable)

## Create a list which contains the supplied matrix together
## with cached values of previous invocations of cacheSolve.
##
## The resulting object (which I call cachable here) will contain the following functions as values:
##    cachable$set(newMatrix) - Call the set method to save a new input matrix in the cachable object
##    cachable$get() - Call the get method to return the current input matrix in this cachable object
##    cachable$set.cached(result, ...) - Save the result of the solve operation together with the used parameters
##    cachable$get.cached(...) - Get the result of the last solve operation if the parameters given
##                               match exactly.

makeCacheMatrix <- function(x = matrix()) {
  # The cached result of the solve operation.
  cached.result <- NULL
  # The parameters that were supplied to solve for 
  # calculating cache.result.
  cached.parameters <- NULL
  
  set <- function(y) {
    x <<- y
    # We get a new matrix so we can discard our cached results.
    cached.result <<- NULL
    cached.parameters <<- NULL
  }
  get <- function() x
  
  set.cached <- function(result, ...) {
    cached.result <<- result
    # We save the parameters so that we only return the
    # cached value if the user supplied the same parameters
    # to the solve method.
    cached.parameters <<- list(...)
  }
  get.cached <- function(...) {
    if (identical(cached.parameters, list(...))) {
      cached.result
    } else {
      NULL
    }
  }
  list(set = set, get = get,
       set.cached = set.cached,
       get.cached = get.cached)
}


## Calls solve on the matrix contained in an list object generated with makeCacheMatrix.
##
## Subsequent calls with the same parameters will use a cached value and not call solve again.
cacheSolve <- function(x, ...) {
  m <- x$get.cached(...)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.cached(m, ...)
  m
}
