## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## This module contains a pair of functions that cache the inverse of a matrix.
## Note: this module assumes that the matrix supplied is always invertible.

## makeCacheMatrix is a function that takes the original matrix,
## computes its inverse using the solve() function and stores
## the result in local memory.
## Note: this function assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(mean) m <<- mean
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
