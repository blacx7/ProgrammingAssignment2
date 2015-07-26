## Final Version
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## This assignment contains of two functions to create and inverse a value of matrix and also to
## check and return a computed inverse matrix from the cache.

## This function will set and get the initial matrix and also inverse the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  
}


## This function will set and return a cache of the inverse value from function "makeCacheMatrix" 
## above if the matrix value has not changed.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
