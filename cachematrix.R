## This function creates a custom matrix object
## which supports caching its inverse, which
## may be retrieved by cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(i) {
    inverse <<- i
  }
  getinverse <- function() inverse
  
  list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Write a short comment describing this function
## Gets the inverse of a matrix created by
## makeCacheMatrix(). Caches the result
## so that future calls will not re-compute if not needed.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  inverse <- solve(x$get())
  x$setinverse(inverse)
  inverse
}
