## This script contains two functions,
## makeCacheMatrix() caches the inverse of a matrix
## cacheSolve() returns the inverse of a matrix from a cache if available
## or calculates it live

## reads in a matrix and stores its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix, first attempting to get it from 
## the cache created from createCacheMatrix, if available.
## Otherwise, computes and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
