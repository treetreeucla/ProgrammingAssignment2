## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.  This pair of 
## functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that 
## can cache its inverse.
##
## Assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  x_inv = NULL
  
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) x_inv <<- inv
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
}
